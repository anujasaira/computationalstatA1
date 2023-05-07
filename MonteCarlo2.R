#Implement two samplers, one for X and one for Y . Plot the histogram and the density
#and comment on the results exploring different values of γ.

#sampler for Pareto
xsampler<- function(n, gamma){
  u<- runif(n)
  1/((1-u)^(1/gamma))
}

#sampler for exponential
ysampler<- function(n,gamma){
  x<- xsampler(n,gamma)
  y<- log(x)
  return(y)
}

x1=xsampler(10000,40)
y1=ysampler(10000,40)
hist(x1,breaks = 30, freq=F)
hist(y1,breaks = 30, freq=F)