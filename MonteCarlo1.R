set.seed(13)



normal_functn_gen = function(sim){
  n<-12 
  Uz<-runif(sim*n,min = -0.5,max = 0.5) 
  Uz<-matrix(Uz,nrow=N,ncol=n) 
  Z<-apply(Uz,1,sum)
  return(Z)
}

N <- 10000

gen_norm<-normal_functn_gen(sim=10000)
mean(gen_norm)
var(gen_norm)




U1 <- runif(N)
U2 <- runif(N)
X1 <- sqrt( -2*log(U1) )*cos(2*pi*U2)

par(mfrow=c(1,2))
hist(Z, main="Normal Distribution", xlab="Value",col="lightblue" ,breaks=30,freq = F,xlim=c(-4,4))
curve(dnorm(x,0,1),add=T)
hist(X1, main="BM Distribution", xlab="Value",col="lightgreen" ,breaks=30,freq = F,xlim=c(-4,4))
curve(dnorm(x,0,1),add=T)


#old and naive algorithm
sum(Z >= 3) / length(Z)

#BM
sum(X1 >= 3) / length(X1)

normgen <- rnorm(10000)
hist(X1, main="Normal Distribution", xlab="Value",col="blue" ,breaks=30,freq = F,xlim=c(-4,4))
curve(dnorm(x,0,1),add=T)
hist(normgen,xlab="Value",col="darkgreen" ,breaks=30,freq = F,xlim=c(-4,4), main = "rnorm Function")
curve(dnorm(x,0,1),add=T)
hist(Z,xlab="Value",col="cyan" ,breaks=30,freq = F,xlim=c(-4,4), main = "rnorm Function")
curve(dnorm(x,0,1),add=T)
exp<- mean(normgen)
var<- var(normgen)



upper_tail_prob<- function(gen_norm,theta){
  prob=sum(gen_norm>theta)/length(gen_norm)
  return(prob)
}
x <- seq(0, 5, by = 0.01)
R<- sapply(seq(0, 5, by = 0.01), function(x) upper_tail_prob(gen_norm,x))
# Calculate cumulative probabilities using pnorm
prob <- 1-pnorm(x,0,1)

# Plot the cumulative probabilities
matplot(x, cbind(prob,R), type = "l", xlab = "x", ylab = "Cumulative Probability", main = "Normal Distribution")
legend("topright", legend = c("pnorm", "old-naive approach"), col = 1:3, lty = 1, bty = "n", cex = 0.8)

