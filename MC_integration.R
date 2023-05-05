set.seed(123)
n <- 1000
f <- function(x) (1 / sqrt(2*pi)) * exp(-(1/x)^2/2) / x^2
curve(f)
integrate(f, 0, .05)
mc <- function(n, a = 0, b = .05){
  
  x= runif(n,a,b)
  mean(f(x))*(b-a)
  
}

ergodic <- function(n_max, a=0, b=0.05){
  
  x = runif(n_max,a,b)
  (b-a)*cumsum(f(x))/c(1:n_max)
  
}


erg = ergodic(1000)
plot(ts(erg))






