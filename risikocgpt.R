# Function to simulate a single battle between attacker and defender units
simulate_battle <- function(attacker_units, defender_units,nsim=10) {
  Results = rep(NA,nsim)
  # Initialize variables
  attacker_losses <- 0
  defender_losses <- 0
  for (i in 1:nsim){
  
  # Simulate battle
  while (attacker_units > 0 && defender_units > 0) {
    # Roll dice for attacker and defender
    attacker_dice <- sort(sample(1:6, min(attacker_units, 3), replace = TRUE, prob = c(1, 1, 1, 1, 1, 1)))
    defender_dice <- sort(sample(1:6, min(defender_units, 2), replace = TRUE, prob = c(1, 1, 1, 1, 1, 1)))
    
    # Compare dice rolls and update losses
    for (i in 1:min(length(attacker_dice), length(defender_dice))) {
      if (attacker_dice[i] > defender_dice[i]) {
        defender_losses <- defender_losses + 1
      } else {
        attacker_losses <- attacker_losses + 1
      }
    }
    
    # Update units
    attacker_units <- attacker_units - defender_losses
    defender_units <- defender_units - attacker_losses
    
    # Reset losses
    attacker_losses <- 0
    defender_losses <- 0
  }
  
  # Return outcome
    Results[i]<-ifelse(defender_units <= 0,1,0)
  }
  return(Results)
}
simulate_battle(10,2)
