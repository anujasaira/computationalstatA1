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

B <- 10000

bootMatrices <- array(0, dim = c(ncol(games), ncol(games),B))

for (i in 1:B) {
  
  bootSample <- games[sample(nrow(games),replace = TRUE),]
  
  bootCov <- cov(bootSample)
  
  bootMatrices[, , i] <- bootCov 
}

standard_errors <- apply(bootMatrices, c(1,2), sd)

means <- apply(bootMatrices, c(1,2), mean)


qnt_CI <- quantile(bootMatrices, probs = c(0.025,0.975))

lower_CLT <- means - 1.96 *standard_errors
upper_CLT <- means + 1.96 *standard_errors

clt_CI <- means + c(-1,1)*1.96*standard_errors

rbind(clt_CI, qnt_CI)




