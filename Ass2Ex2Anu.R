library(boot)
library(tidyverse)


songs <- readRDS("spot (1).RDS")
head(songs)


 

boxplot(popularity~first_auth,data=songs)

u_art<- unique(songs$first_auth)


X <- songs$pop[songs$first_auth == "AC/DC"]

# Extract popularity measurements for Artist Y
Y <- songs$pop[songs$first_auth == "Kanye West"]


med_logit_X <-median( logit(X))
med_logit_Y <-median( logit(Y))

T_XY <- abs(med_logit_X-med_logit_Y)

pop_diff <- matrix(0,nrow = length(u_art),ncol = length(u_art))
for (i in 1:length(u_art)){
  for(j in 1:length(u_art)){
    X <- songs$pop[songs$first_auth == u_art[j]]
    Y <- songs$pop[songs$first_auth ==  u_art[i]]
    med_logit_X <-median( logit(X))
    med_logit_Y <-median( logit(Y))
    pop_diff[i,j]<- abs(med_logit_X-med_logit_Y)
  }
}

pop_diff


p_test<- data.frame(
  Artistx<- rep(u_art,1),
  Artisty<- rep(u_art,1),
  win_prob<- pop_diff
)
length(win_prob)

ggplot(p_test,aes(x=Artistx,y=Artisty,fill=win_prob))+
  geom_tile()+
  scale_fill_gradient(low="red",high="darkgreen")+
  labs(title = "Attacker Win Probability",x="Defender Units",y="Attacker Units")+
  geom_tile(color = "white",lwd = 0.5,linetype = 1)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


# Compute all the possible permutations
perms     = do.call("rbind",permn(40))
N         = nrow(perms)
perm_stat = numeric(N)

perm_stat= apply(perms,1, function(x) { perm_outcome = songs$pop[x]
  mean(perm_outcome[D$lab == "US"])-
  mean(perm_outcome[D$lab == "EU"])
})
hist(perm_stat,breaks = 20)
abline(v = obs_diff, lwd = 3, col = 2, lty = 2)



boxplot( songs$pop ~ songs$first_auth,
         col= cutree( hclust(as.dist(1-pop_diff)),5))