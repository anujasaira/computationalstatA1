set.seed(12)
N<-10000
n<-12 
Uz<-runif(N*n,min = -0.5,max = 0.5) 
Uz<-matrix(Uz,nrow=N,ncol=n) 
Z<-apply(Uz,1,sum)
Zmean<- mean(Z)
Zmean

EX_2 <- Zmean^2
X2<- Z^2
EX2 <- mean(X2)
varX <- EX2 - EX_2
varX
var(Z)

#code to be deleted

variance <- sum((Z - mean(Z))^2) / (length(Z) - 1)
variance


N <- 10000
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
par(mfrow=c(1,3))
hist(X1, main="Normal Distribution", xlab="Value",col="blue" ,breaks=30,freq = F,xlim=c(-4,4))
curve(dnorm(x,0,1),add=T)
hist(normgen,xlab="Value",col="darkgreen" ,breaks=30,freq = F,xlim=c(-4,4), main = "rnorm Function")
curve(dnorm(x,0,1),add=T)
hist(Z,xlab="Value",col="cyan" ,breaks=30,freq = F,xlim=c(-4,4), main = "rnorm Function")
curve(dnorm(x,0,1),add=T)
exp<- mean(normgen)
var<- var(normgen)
