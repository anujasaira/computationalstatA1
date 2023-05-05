
set.seed(123)
combat_round <- function(def_units, att_units,sim=1000) {
  Results = rep(NA,sim)
  AS<-att_units
  DS<-def_units
  for(i in 1:N){
    while(def_units>0 & att_units>0){
      Dnum <- sort(sample(1:6, min(def_units,3),replace = TRUE),decreasing = T)
      Anum <- sort(sample(1:6, min(att_units,3),replace = TRUE),decreasing = T)
      for (j in 1:min(length(Dnum),length(Anum))){
        if(Anum[j]>Dnum[j]){
          def_units<-def_units-1
        }
        else{
          att_units<-att_units-1
        }
      }
      
    }
    Results[i]<- ifelse(att_units>0,1,0)
    att_units<-AS
    def_units<-DS
  }
  return(mean(Results))
    
    
   
}





print(combat_round(def_units=1,att_units=2,sim=10000))


