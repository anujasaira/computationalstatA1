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


##Non va la plot
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
cumulative_proportion <- cumsum(eigen_values) / sum(eigen_values)

jstar <- min(which(cumulative_proportion > 0.76))
jstar

#Point e
total_sum <- sum(eigen_values)

bias <- rep(0, length(eigen_values))
standard_error <- rep(0, length(eigen_values))

for (i in 1:B) {
  # Generate bootstrap sample by sampling with replacement
  boot_theta_j <- sample(eigen_values, replace = TRUE)
  
  # Calculate the bootstrap estimator for each parameter θj
  for (j in 1:length(eigen_values)) {
    theta_j <- sum(boot_theta_j[1:j]) / total_sum
    
    # Calculate bias and update the estimate
    bias[j] <- bias[j] + (theta_j - cumulative_proportion[j])
    
    # Calculate squared differences and update standard error
    standard_error[j] <- standard_error[j] + (theta_j - cumulative_proportion[j])^2
  }
}



bias <- bias / B
bias

standard_error <- sqrt(standard_error / (B - 1))
standard_error



bias_jstar <- rep(0, jstar)
std_error_jstar <- rep(0, jstar)

# Perform bootstrap sampling and calculations
for (i in 1:B) {
  # Generate bootstrap sample by sampling with replacement
  boot_theta_jstar <- sample(eigen_values, replace = TRUE)
  
  # Calculate the bootstrap estimator for jstar
  theta_jstar <- sum(boot_theta_jstar[1:jstar]) / total_sum
  
  # Calculate bias and update the estimate
  bias_jstar <- bias_jstar + (theta_jstar - cumulative_proportion[1:jstar])
  
  # Calculate squared differences and update standard error
  std_error_jstar <- std_error_jstar + (theta_jstar - cumulative_proportion[1:jstar])^2
}

# Calculate average bias and standard error across bootstrap samples
bias_jstar <- bias_jstar / B
std_error_jstar <- sqrt(std_error_jstar / (B - 1))


bootstrap_estimate <- cumulative_proportion[jstar] + bias_jstar[jstar]
bootstrap_estimate
bootstrap_std_error <- std_error_jstar[jstar]
bootstrap_std_error

#I don't know how to do the probability 
p_jstar_5 <- mean(boot_theta_jstar==5)
p_jstar_5

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
  
set.seed(123)  # Set a seed for reproducibility

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
  
  
  
  
  

