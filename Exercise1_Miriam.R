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

set.seed(12345)

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
cumulative_proportion <- cumsum(eigen_values) / sum(eigen_values)

jstar <- min(which(cumulative_proportion > 0.76))





 




