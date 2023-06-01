library(boot)
library(tidyverse)


songs <- readRDS("spot (1).RDS")
head(songs)


 

boxplot(popularity~first_auth,data=songs)

u_art<- unique(songs$first_auth)


X <- songs[songs$first_auth == "AC/DC","pop"]

# Extract popularity measurements for Artist Y
Y <- songs$pop[songs$first_auth == "Kanye West"]

songs[songs$first_auth == "Kanye West"]


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


####################################################################################################


popdiff<- function(ply1,ply2){
  play_pop_diff<- abs(median(logit(songs$pop[songs$first_auth == u_art[ply1]]))-median(logit(songs$pop[songs$first_auth ==  u_art[ply2]])))
  return(play_pop_diff)
  }


pop_diff_T <- function(){
  res<-sapply(1:40, function(x) {
    sapply(1:40, function(y) {
      popdiff(x,y)
    })
  })
  return(res)
}


pop_diff

pop_diff<- pop_diff_T()

rownames(pop_diff) <- u_art
colnames(pop_diff) <- u_art


p_test<- data.frame(
  Artistx<- u_art,
  Artisty<- u_art,
  pop_diff<- as.vector(pop_diff_T())
)




##########################################################################################################

library(reshape2)


max(pop_diff)

ggplot(melt(pop_diff),aes(Var1,Var2))+
         geom_tile(aes(fill = value), colour = "white")  +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.text.x = element_text(angle=90,hjust = 1,siz=5),
        axis.text.y = element_text(hjust = 1,siz=5))




#+
#  scale_fill_gradient(low="red",high="darkgreen")+
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




heatmap(diff_matrix, col = heat.colors(10))


# Popularity ratings for two artists
artist1 <- c(5, 7, 3, 6, 8)
artist2 <- c(4, 6, 2, 5, 7)

# Function to calculate pairwise differences
diff_function <- function(x, y) {
  abs(x - y)
}

# Create a matrix of pairwise differences
diff_matrix <- outer(artist1, artist2, FUN = diff_function)

# Plot the matrix as a heatmap
heatmap(pop_diff, col = heat.colors(1600))


superheat::superheat(pop_diff)











data<- songs


library(combinat)
p_values <- matrix(0,nrow = length(u_art),ncol = length(u_art))
for (i in 1:length(u_art)){
  for(j in 1:length(u_art)){
    X <- data[data$first_auth ==  u_art[i],"pop"]
    Y <- data[data$first_auth ==  u_art[j],"pop"]
    observed_stat=popdiff(i,j)
    
    
    
    num_perm<- 1000
    permutation_stat<- numeric(num_perm)
    for (k in 1:num_perm){
      combined<- union_all(X,Y)
      combined$pop <- sample(combined$pop,nrow(combined),replace = F)
      perm_X<- combined$pop[1:length(X$pop)]
      perm_Y<-combined$pop[(length(X$pop)+1):(length(X$pop)+length(Y$pop))]
      permutation_stat[k]<- abs(median(logit(perm_X))-median(logit(perm_Y)))
      
    }
    
    p_value <- mean(permutation_stat>=observed_stat)
    
    p_values[i,j]<- p_value
    
    
  }
}



p_test <- function(){
  res<-sapply(1:40, function(x) {
    sapply(1:40, function(y) {
      if(x>=y){
      X <- data[data$first_auth ==  u_art[x],"pop"]
      Y <- data[data$first_auth ==  u_art[y],"pop"]
      observed_stat=popdiff(x,y)
      combined<- union_all(X,Y)
      
      
      
      num_perm<- 10000
      permutation_stat<- numeric(num_perm)
      for (k in 1:num_perm){
        if(i>=j){
        
        combined$pop <- sample(combined$pop,nrow(combined),replace = F)
        perm_X<- combined$pop[1:length(X$pop)]
        perm_Y<-combined$pop[(length(X$pop)+1):(length(X$pop)+length(Y$pop))]
        permutation_stat[k]<- abs(median(logit(perm_X))-median(logit(perm_Y)))
        
      }
      
      p_value <- mean(permutation_stat>=observed_stat)
      
      p_values[x,y]<- p_value
      p_values[y,x]<- p_value
      }
      }
    })
  })
  return(p_values)
}

factorial(22)

ptestval<- p_test()

p_popdiff<- function(ply1,ply2){
  observed_stat=popdiff(i,j)
  
  
  
  
  play_pop_diff<- abs(median(logit(songs$pop[songs$first_auth == u_art[ply1]]))-median(logit(songs$pop[songs$first_auth ==  u_art[ply2]])))
  return(play_pop_diff)
}











B = 50000
perm_stat = numeric(B)

perms_subs = replicate(B, sample(1:100,replace = F))


perm_stat = apply(perms_subs,1, function(x) { perm_outcome = D$inc[x]
t.test(x = perm_outcome[D$lab == "US"],
       y = perm_outcome[D$lab == "EU"])$stat})


