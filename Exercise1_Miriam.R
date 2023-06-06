games <- readRDS("games_preprocessed.RDS")
games <- games[,-c(1,2)]
games <- scale(games)
cov_matrix <- cov(games)
heatmap(cov_matrix)
cor_matrix <- cor(games)
plot(cov_matrix, cor_matrix, 
     xlab = "Variance-Covariance Matrix",
     ylab = "Correlation Matrix")
abline(0,1,col = "red")

set.seed(1234)

B <- 1000

boot <- function(B, games){
  
  results <- matrix(NA, nrow = B, ncol = ncol(games)^2)
  bootstrap <- replicate(B, {
    
    bootSample <- games[sample(nrow(games), replace = T),]
    bootCov <- cov(bootSample)
    i <- seq_len(B)
    results[i,] <- as.vector(bootCov)
  })
  return(bootstrap)
}

boot <- boot(1000,games)

standard_errors <- apply(boot, 1, sd)

means <- apply(boot, 1, mean)



qnt_CI <- t(apply(boot, 1, function(z) quantile(z, probs = c(0.025,0.975)))
)

clt_CI <- means + c(-1,1)*1.96*standard_errors

rbind(clt_CI, qnt_CI)



plot_results <- function(clt_CI, qnt_CI) {
  n <- length(clt_CI)
  plot(1:n, clt_CI[, 1], type = "l", ylim = range(c(clt_CI, qnt_CI)), 
       xlab = "Entry", ylab = "Confidence Interval", main = "Bootstrap Confidence Intervals")
  lines(1:n, clt_CI[, 2], type = "l", col = "blue")
  lines(1:n, qnt_CI[, 1], type = "l", col = "green")
  lines(1:n, qnt_CI[, 2], type = "l", col = "purple")
  legend("topright", legend = c("CLT Lower", "CLT Upper", "Quantile Lower", "Quantile Upper"),
         col = c("blue", "blue", "green", "purple"), lty = c(1, 1, 1, 1))
}


eigen_values <- eigen(cov_matrix)$values
theta <- cumsum(eigen_values) / sum(eigen_values)

jstar <- min(which(theta > 0.76))
jstar

#Point e


boot_theta <- matrix(0, nrow = B, ncol = length(theta))
boot_jstar <- numeric(B)

for (i in 1:B) {
  # Generate bootstrap sample by resampling from the original df
  boot_sample <- games[sample(nrow(games), replace= TRUE),]
  
  # Calculate covariance matrix and eigenvalues for the bootstrap
  cov_boot <- cov(boot_sample)
  eigen_boot <- eigen(cov_boot)$values
  
  #Compute theta for the bootstrap sample
  boot_theta[i,] <- cumsum(eigen_boot) / sum(eigen_boot)
  
  #Compute jstar for the bootstrap sample
  boot_jstar[i] <- min(which(boot_theta[i,] > 0.76))
  
}

bias <- colMeans(boot_theta)-theta
se <- apply(boot_theta, 2, sd)
bias
se
bias_jstar <- mean(boot_jstar) - jstar
se_jstar <- sd(boot_jstar)
bias_jstar
se_jstar
p_jstar <- mean(boot_jstar==5)
p_jstar


#Point f 
set.seed(abs(636-555-3226))
ind <- sample(1:nrow(games),5000,FALSE)
sub_data <- games[ind,]

sub_data <- as.data.frame(sub_data)
regression <- lm(sub_data$average_rating~., data = sub_data)
summary(regression)  

#point g
#PAIRED BOOTSTRAP
num_coeffs <- length(coef(regression))
bootstrap_estimates <- matrix(0, nrow = B, ncol = num_coeffs)


for (i in 1:B) {
  # Generate bootstrap sample indices by sampling with replacement
  bootstrap_indices <- sample(nrow(sub_data), replace = TRUE)
  
  # Create bootstrap sample using the selected indices
  bootstrap_sample <- sub_data[bootstrap_indices, ]
  
  # Fit the regression model on the bootstrap sample
  bootstrap_regression <- lm(average_rating ~ ., data = bootstrap_sample)
  
  # Store the coefficients of the bootstrap regression model
  bootstrap_estimates[i, ] <- coef(bootstrap_regression)
}

coefficient_bias <- apply(bootstrap_estimates, 2, mean) - coef(regression)
coefficient_bias
coefficient_std_error <- apply(bootstrap_estimates, 2, sd)
coefficient_std_error
coefficient_confidence_intervals <- t(apply(bootstrap_estimates, 2, function(x) quantile(x, c(0.025, 0.975))))
coefficient_confidence_intervals

adjusted_r_squared <- numeric(B)
for (i in 1:B) {
  # Generate bootstrap sample indices by sampling with replacement
  bootstrap_indices <- sample(nrow(sub_data), replace = TRUE)
  
  # Create bootstrap sample using the selected indices
  bootstrap_sample <- sub_data[bootstrap_indices, ]
  
  # Fit the regression model on the bootstrap sample
  bootstrap_regression <- lm(average_rating ~ ., data = bootstrap_sample)
  
  # Calculate the adjusted R-squared for the bootstrap regression model
  adjusted_r_squared[i] <- 1 - (1 - summary(bootstrap_regression)$r.squared) * ((nrow(bootstrap_sample) - 1) / (nrow(bootstrap_sample) - length(coef(bootstrap_regression)) - 1))
}
adjusted_r_squared_bias <- mean(adjusted_r_squared) - summary(regression)$adj.r.squared
adjusted_r_squared_bias
adjusted_r_squared_std_error <- sd(adjusted_r_squared)
adjusted_r_squared_std_error
adjusted_r_squared_confidence_interval <- quantile(adjusted_r_squared, c(0.025, 0.975))
adjusted_r_squared_confidence_interval
###############################################à
#Point h
regr_jack <- function(df, B){
  
  n_vars <- ncol(df)
  n_obs <- nrow(df)
  
  betas <- matrix(NA,nrow = B, ncol = n_vars)
  R2_adj <- numeric(B)

  
  for (i in 1:B) {
    j_sample <- df[-i,]
    model <- lm(average_rating~., data = j_sample)
    s <- summary(model)
    betas[i,] <- model$coefficients
    R2_adj[i] <- s$adj.r.squared

  }
  return(list(betas=betas, R2_adj=R2_adj))
}
res_j <- regr_jack(sub_data, 1000)





perform_paired_bootstrap <- function(data, B) {
  num_coeffs <- length(coef(regression))
  bootstrap_estimates <- matrix(0, nrow = B, ncol = num_coeffs)
  
  for (i in 1:B) {
    bootstrap_indices <- sample(nrow(data), replace = TRUE)
    bootstrap_sample <- data[bootstrap_indices, ]
    bootstrap_regression <- lm(average_rating ~ ., data = bootstrap_sample)
    bootstrap_estimates[i, ] <- coef(bootstrap_regression)
  }
  
  coefficient_bias <- apply(bootstrap_estimates, 2, mean) - coef(regression)
  coefficient_std_error <- apply(bootstrap_estimates, 2, sd)
  coefficient_confidence_intervals <- t(apply(bootstrap_estimates, 2, function(x) quantile(x, c(0.025, 0.975))))
  
  adjusted_r_squared <- numeric(B)
  for (i in 1:B) {
    bootstrap_indices <- sample(nrow(data), replace = TRUE)
    bootstrap_sample <- data[bootstrap_indices, ]
    bootstrap_regression <- lm(average_rating ~ ., data = bootstrap_sample)
    adjusted_r_squared[i] <- 1 - (1 - summary(bootstrap_regression)$r.squared) * ((nrow(bootstrap_sample) - 1) / (nrow(bootstrap_sample) - length(coef(bootstrap_regression)) - 1))
  }
  adjusted_r_squared_bias <- mean(adjusted_r_squared) - summary(regression)$adj.r.squared
  adjusted_r_squared_std_error <- sd(adjusted_r_squared)
  adjusted_r_squared_confidence_interval <- quantile(adjusted_r_squared, c(0.025, 0.975))
  
  theta_estimates <- numeric(B)
  coeff <- coef(regression)
  
  for (i in 1:B) {
    boot_sample <- data[sample(nrow(data), replace = TRUE), ]
    regression_boot <- lm(average_rating ~ ., data = boot_sample)
    coeff_boot <- coef(regression_boot)
    theta_boot <- max(((coeff_boot[5] - coeff_boot[6])/(coeff_boot[3]+coeff_boot[2])), 0)
    theta_estimates[i] <- theta_boot
  }
  
  bias <- mean(theta_estimates) - (coeff[5] - coeff[6])/(coeff[3]+coeff[2])
  se <- sd(theta_estimates)
  ci <- quantile(theta_estimates, probs = c(0.025, 0.975))
  
  results <- list(
    coefficient_bias = coefficient_bias,
    coefficient_std_error = coefficient_std_error,
    coefficient_confidence_intervals = coefficient_confidence_intervals,
    adjusted_r_squared_bias = adjusted_r_squared_bias,
    adjusted_r_squared_std_error = adjusted_r_squared_std_error,
    adjusted_r_squared_confidence_interval = adjusted_r_squared_confidence_interval,
    theta_bias = bias,
    theta_std_error = se,
    theta_confidence_interval = ci
  )
  
  return(results)
}

results <- perform_paired_bootstrap(sub_data, B)
results






```{r}
# Jackknife approach
num_coeffs <- length(coef(regression))
jackknife_estimates <- matrix(0, nrow = nrow(sub_data), ncol = num_coeffs)

for (i in 1:nrow(sub_data)) {
  # Create jackknife sample by excluding one observation
  jackknife_sample <- sub_data[-i, ]
  
  # Fit the regression model on the jackknife sample
  jackknife_regression <- lm(average_rating ~ ., data = jackknife_sample)
  
  # Store the coefficients of the jackknife regression model
  jackknife_estimates[i, ] <- coef(jackknife_regression)
}

coefficient_bias_jack <- rowMeans(jackknife_estimates) - coef(regression)
coefficient_bias_jack
coefficient_std_error_jack <- sqrt(((nrow(sub_data) - 1) / nrow(sub_data)) * apply((jackknife_estimates - rowMeans(jackknife_estimates))^2, 2, sum))
coefficient_std_error_jack
coefficient_confidence_intervals_jack <- t(apply(jackknife_estimates, 2, function(x) {
  mean_x <- mean(x)
  se_x <- sqrt(((nrow(sub_data) - 1) / nrow(sub_data)) * sum((x - mean_x)^2))
  ci_lower <- mean_x - 1.96 * se_x
  ci_upper <- mean_x + 1.96 * se_x
  c(ci_lower, ci_upper)
}))
coefficient_confidence_intervals_jack

# Calculate adjusted R-squared using the jackknife approach
adjusted_r_squared_jack <- numeric(nrow(sub_data))

for (i in 1:nrow(sub_data)) {
  # Create jackknife sample by excluding one observation
  jackknife_sample <- sub_data[-i, ]
  
  # Fit the regression model on the jackknife sample
  jackknife_regression <- lm(average_rating ~ ., data = jackknife_sample)
  
  # Calculate the adjusted R-squared for the jackknife regression model
  adjusted_r_squared_jack[i] <- 1 - (1 - summary(jackknife_regression)$r.squared) * ((nrow(sub_data) - 1) / (nrow(sub_data) - length(coef(jackknife_regression)) - 1))
}

adjusted_r_squared_CI_jack <- quantile(adjusted_r_squared_jack, c(0.025, 0.975))

adjusted_r_squared_CI_jack

# Calculate theta_hat using the jackknife approach
coeff <- coef(regression)
theta_hat_jack <- apply(jackknife_estimates, 1, function(x) max(((coeff[5] - coeff[6]) / (coeff[3] + coeff[2])), 0))

theta_hat_CI_jack <- quantile(theta_hat_jack, c(0.025, 0.975))
theta_hat_CI_jack

```

  
  
  
  

