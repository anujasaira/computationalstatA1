#Implement two samplers, one for X and one for Y. Plot the histogram and the density
#and comment on the results exploring different values of γ.
library(patchwork)
#sampler for Pareto
xsampler<- function(n, gamma){
  u<- runif(n)
  x<-1/((1-u)^(1/gamma))
  return(x)
}

#sampler for Exponential
ysampler<- function(x){
  y<- log(x)
  return(y)
}
i=0
gamma = c(7,15,25)

x1=xsampler(10000,10)
y1=ysampler(x1)
x2<-xsampler(10000,10)
y2<- ysampler(x2)

library(ggplot2)

ggplot(data.frame(x=xsampler(10000,gamma[1])), aes(x)) +
  geom_histogram(aes(y=..density..), fill="gray",col="darkblue")+
  geom_density()
ggplot(data.frame(x=xsampler(10000,gamma[2])), aes(x)) +
  geom_histogram(aes(y=..density..), fill="gray",col="darkblue")+
  geom_density()
ggplot(data.frame(x=xsampler(10000,gamma[3])), aes(x)) +
  geom_histogram(aes(y=..density..), fill="gray",col="darkblue")+
  geom_density()

