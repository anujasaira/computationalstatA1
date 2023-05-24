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


B <- 10000

bootMatrices <- array(0, dim = c(ncol(games), ncol(games),B))

for (i in 1:B) {
  
  bootSample <- games[sample(nrow(games),replace = TRUE),]
  
  bootCov <- cov(bootSample)
  
  bootMatrices[, , i] <- bootCov 
}

standard_errors <- apply(bootMatrices, c(1,2), sd)