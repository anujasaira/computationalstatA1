myfunction <- function(y){ 
  f  = (1/sqrt(2*pi))*(1/y^2)*exp(-(1/(2*Y^2)))
  return(f)
  
  
  }

  


n <- 2000
n
a <- -20
b <-  20
u <-runif(n, a, b)

H          <- (b-a) * myfunction(u) 
psi_hat_MC <- mean(H)
MCE        <- sd(H)/sqrt(n)
