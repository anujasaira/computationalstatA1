#new R script

# # Define number of simulations
# n_sim <- 10000
# 
# # Define initial army sizes
# attacker_armies <- 2
# defender_armies <- 1
# 
# # Define function to simulate one round of combat
# combat_round <- function(attacker_armies, defender_armies) {
#   # Roll attacker's dice
#   attacker_rolls <- sort(sample(1:6, min(attacker_armies, 3), replace = TRUE), decreasing = TRUE)
#   
#   # Roll defender's dice
#   defender_rolls <- sort(sample(1:6, min(defender_armies, 2), replace = TRUE), decreasing = TRUE)
#   
#   # Compare highest dice rolls and remove losing armies
#   if(attacker_rolls[1] > defender_rolls[1]) {
#     defender_armies <- defender_armies - 1
#   } else {
#     attacker_armies <- attacker_armies - 1
#   }
#   
#   # Compare second-highest dice rolls (if applicable) and remove losing armies
#   if(length(attacker_rolls) > 1 & length(defender_rolls) > 1) {
#     if(attacker_rolls[2] > defender_rolls[2]) {
#       defender_armies <- defender_armies - 1
#     } else {
#       attacker_armies <- attacker_armies - 1
#     }
#   }
#   
#   # Return updated army sizes
#   return(list(attacker_armies = attacker_armies, defender_armies = defender_armies))
# }
# 
# # Run simulations
# attacker_wins <- 0
# for(i in 1:n_sim) {
#   result <- combat_round(attacker_armies, defender_armies)
#   while(result$attacker_armies > 0 & result$defender_armies > 0) {
#     result <- combat_round(result$attacker_armies, result$defender_armies)
#   }
#   if(result$attacker_armies > 0) {
#     attacker_wins <- attacker_wins + 1
#   }
# }
# 
# # Calculate and print results
# attacker_win_prob <- attacker_wins / n_sim
# cat("Attacker win probability:", round(attacker_win_prob, 3))

set.seed(123)
N<-10000
#DA<-1
#AA<-2
combat_round <- function(DA, AA,N=1000) {
  Results = rep(NA,N)
  AS<-AA
  DS<-DA
  for(i in 1:N){
    while(DA>0 & AA>0){
      Dnum <- sort(sample(1:6, min(AA,3),replace = TRUE),decreasing = T)
      Anum <- sort(sample(1:6, min(DA,3),replace = TRUE),decreasing = T)
      for (j in 1:min(length(Dnum),length(Anum))){
        if(Anum[j]>Dnum[j]){
          DA<-DA-1
        }
        else{
          AA<-AA-1
        }
      }
      
    }
    Results[i]<- ifelse(AA>0,1,0)
    AA<-AS
    DA<-DS
  }
  return(mean(Results))
    
    
   
}





print(combat_round(DA=1,AA=10,N))


