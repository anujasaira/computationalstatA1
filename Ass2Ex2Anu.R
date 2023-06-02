###################################################################################################

library(boot)
library(tidyverse)
library(reshape2)
library(combinat)



songs <- readRDS("spot (1).RDS")
head(songs)

boxplot(popularity~first_auth,data=songs)

u_art<- unique(songs$first_auth)

#punto B

X <- songs$pop[songs$first_auth == "AC/DC"]

# Extract popularity measurements for Artist Y
Y <- songs$pop[songs$first_auth == "Kanye West"]

med_logit_X <-median( logit(X))
med_logit_Y <-median( logit(Y))

T_XY <- abs(med_logit_X-med_logit_Y)

pop_diff_T <- function(){
  res<-sapply(1:40, function(x) {
    sapply(1:40, function(y) {
      abs(median(logit(songs$pop[songs$first_auth == u_art[x]]))-median(logit(songs$pop[songs$first_auth ==  u_art[y]])))
    })
  })
  return(res)
}

pop_diff<- pop_diff_T()

rownames(pop_diff) <- u_art
colnames(pop_diff) <- u_art

ggplot(melt(pop_diff),aes(Var1,Var2))+
  geom_tile(aes(fill = value), colour = "white")  +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.text.x = element_text(angle=90,hjust = 1,siz=5),
        axis.text.y = element_text(hjust = 1,siz=5))


#punto c

p_values <- matrix(0,nrow = length(u_art),ncol = length(u_art))
for (i in 1:length(u_art)){
  for(j in 1:length(u_art)){
    if(i>=j){
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
      p_values[j,i]<- p_value
      
      
    }
  }
}


p_values

rownames(p_values) <- u_art
colnames(p_values) <- u_art

ggplot(melt(p_values),aes(Var1,Var2))+
  geom_tile(aes(fill = value), colour = "white")  +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.text.x = element_text(angle=90,hjust = 1,siz=5),
        axis.text.y = element_text(hjust = 1,siz=5))


#punto d

boxplot( data$pop ~ data$first_auth,
         col= cutree( hclust(as.dist(1-p_values)),5))


#punto e
pop_scaled <- scale(data$pop)
df<- data.frame(Artist = data$first_auth,pop_scaled)

t_test<- function(df,artist){
  x<- df[df$Artist==artist,"pop_scaled"]
  t_test<- t.test(x,mu=0,alternative = "greater")
  return(t_test$p.value)
}

p_values<- sapply(unique(df$Artist),t_test,df=df)

prob_violation <- sum(p_values<0.10)/length(p_values)

plot(sort(p_values), type="l",col = "blue", main = "Density Plot of P-values", xlab = "P-values", ylab = "Density", ylim=c(0,2))


#punto f
adjusted_bh <- p.adjust(p_values, method = "BH")
adjusted_bonferroni <- p.adjust(p_values, method = "bonferroni")
adjusted_holm <- p.adjust(p_values, method = "holm")
results <- data.frame(p_values, adjusted_bh, adjusted_bonferroni,adjusted_holm)

# Plotting the p-values and adjusted p-values
plot(sort(p_values), type="l", col = "blue", xlab = "Observation", ylab = "P-value",
     main = "Comparison of P-values and Adjusted P-values")
lines(sort(adjusted_bh), col = "red")
lines(sort(adjusted_bonferroni),  col = "yellow")
lines(sort(adjusted_holm), col = "green")
abline(h = 0.05, lwd = 1, lty = 2)
legend("bottomright", legend = c("P-values", "Adjusted (BH)", "Adjusted (Bonferroni)","Adjusted (Holm)"),
       col = c("blue", "red","yellow","green"), lwd=1,)



##########################################################################################################





# Creating a square matrix
matrix_data <- matrix(5:8, nrow = 2)

# Converting the matrix to a distance object
dist_object <- as.dist(matrix_data)

# Printing the distance object
print(dist_object)



library(reshape2)


max(pop_diff)

ggplot(melt(pop_diff),aes(Var1,Var2))+
         geom_tile(aes(fill = value), colour = "white")  +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.text.x = element_text(angle=90,hjust = 1,siz=5),
        axis.text.y = element_text(hjust = 1,siz=5))


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


p_values




p_values <- matrix(0,nrow = length(u_art),ncol = length(u_art))
for (i in 1:length(u_art)){
  for(j in 1:length(u_art)){
    if(i>=j){
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
    p_values[j,i]<- p_value
    
    
    }
  }
}


p_values

rownames(p_values) <- u_art
colnames(p_values) <- u_art

ggplot(melt(p_values),aes(Var1,Var2))+
  geom_tile(aes(fill = value), colour = "white")  +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.text.x = element_text(angle=90,hjust = 1,siz=5),
        axis.text.y = element_text(hjust = 1,siz=5))




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
        if(x>=y){
        
        combined$pop <- sample(combined$pop,nrow(combined),replace = F)
        perm_X<- combined$pop[1:length(X$pop)]
        perm_Y<-combined$pop[(length(X$pop)+1):(length(X$pop)+length(Y$pop))]
        permutation_stat[k]<- abs(median(logit(perm_X))-median(logit(perm_Y)))
      p_value <- mean(permutation_stat>=observed_stat)
      
      p_values[x,y]<- p_value
      p_values[y,x]<- p_value
        }
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




B = 50000
perm_stat = numeric(B)

perms_subs = replicate(B, sample(1:100,replace = F))


perm_stat = apply(perms_subs,1, function(x) { perm_outcome = D$inc[x]
t.test(x = perm_outcome[D$lab == "US"],
       y = perm_outcome[D$lab == "EU"])$stat})


