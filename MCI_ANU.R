library(tidyverse)
library(patchwork)
library(mixtools)
theme_set(theme_bw())

set.seed(155523)
n <- 1000000
a <- 0
b <-  0.05 
u <-runif(n, a, b)

intfunc <- function(y){
  f = (1/sqrt(2*pi)) * (1/(y^2)) * (exp(-1/(2*(y^2))))
}



H1 = (b-a)* intfunc(u)
psi_hat_MC <- mean(H1) 
psi_hat_MC



set.seed(1234)
i_1n <- 1:n
plot( i_1n, cummean(H1), type='l', lwd=1,  
      xlab="Sample Size N", ylab="Integral Value",col=1)
abline(h=integrate(intfunc, a, b)$value, lwd=1, col=2, lty=2)




H2 <- (b-a)*intfunc(0.05-u) 
MC1 <- mean(H1)
MC2 <- mean(H2)

MCA <- (MC1+MC2)/2


MCE_MC1 <- sd(H1)/sqrt(n);MCE_MC1 
MCE_MC2 <- sd(H2)/sqrt(n);MCE_MC2 

MCE_MCA <- sqrt(1+cor(H1,H2))*sd(H1)/sqrt(2*n);MCE_MCA 


effgain<- MCE_MCA/MCE_MC1;effgain




integrate(intfunc, a, b)
pnorm(20,0,1,lower.tail = F)

