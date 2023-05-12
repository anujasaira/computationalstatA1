set.seed(123)

myfunction <- function(y){ 
  f  = (1/sqrt(2*pi))*(1/y^2)*exp(-(1/(2*y^2)))
  return(f)
  
  
  }

  


n <- 100000
n
a <- -0
b <-  1/20
u <-runif(n, a, b)

H          <- (b-a) * myfunction(u) 
psi_hat_MC <- mean(H)
MCE        <- sd(H)/sqrt(n)



library(tidyverse)
set.seed(123)
i_1n <- 1:n
plot( i_1n, cummean(H), type='l', lwd=1,  
      xlab="Sample Size N", ylab="Integral Value",col=1)
abline(h=integrate(myfunction, a, b)$value, lwd=1, col=2, lty=2)

integrate(myfunction, a, b) 

set.seed(123)
u   <- runif(n, a, b)
H1 <- (b-a)*h(u) 
H2 <- (b-a)*h(0.05-u) 
MC1 <- mean(H1)
MC2 <- mean(H2)
MCA <- (MC1+MC2)/2

MCE_MC1 <- sd(H1)/sqrt(n);MCE_MC1 


integrate(myfunction, a, b)
pnorm(20,0,1,lower.tail=F)
