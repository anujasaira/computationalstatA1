library(ggplot2)

set.seed(123)
combat_round <- function(att_units,def_units,sim=10000) {
  Results = rep(NA,sim)
  AS<-att_units
  DS<-def_units
  for(i in 1:sim){
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

prob_att_win <- function(){
  res<-sapply(1:10, function(x) {
  sapply(1:10, function(y) {
    combat_round(y,x,1000)
    })
  })
  return(res)
}
# Print the result
#print(result)


att_prob<- data.frame(
  attacker_unit<- rep(1:10,10),
  defender_unit<- rep(1:10,each= 10),
  win_prob<- as.vector(prob_att_win())
)


#go through outer 
ggplot(att_prob,aes(x=defender_unit,y=attacker_unit,fill=win_prob))+
  geom_tile()+
  scale_fill_gradient(low="red",high="darkgreen")+
  labs(title = "Attacker Win Probability",x="Defender Units",y="Attacker Units")+
  geom_tile(color = "white",lwd = 0.5,linetype = 1)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))



print(combat_round(def_units=1,att_units=2,sim=10000))


