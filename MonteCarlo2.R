#Implement two samplers, one for X and one for Y. Plot the histogram and the density
#and comment on the results exploring different values of γ.

#sampler for Pareto
xsampler<- function(n, gamma){
  u<- runif(n)
  x<-1/((1-u)^(1/gamma))
  return(x)
}

#sampler for Exponential
ysampler<- function(n,gamma){
  x<- xsampler(n,gamma)
  y<- log(x)
  return(y)
}

x1=xsampler(10000,40)
y1=ysampler(10000,40)
#hist(x1,breaks = 30, freq=F)
#gamma<-40
#curve((gamma/x)^(gamma+1), from=1, to=10, col="blue", x_lab="x", ylab="density", main="Pareto density")
#hist(y1,breaks = 30, freq=F)


library(ggplot2)
ggplot(data.frame(x=x1), aes(x = x1)) +
  geom_histogram(aes(y=..density..), fill="gray",col="darkblue")


