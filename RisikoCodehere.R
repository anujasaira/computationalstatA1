set.seed(123)
combat_round <- function(def_units, att_units,sim=1) {
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



result2 <- function(){
  att_unit <- 1:10
  def_unit <- 1:10
  res<-matrix(nrow=length(att_unit),ncol =length(def_unit))
  
  for(i in 1:length(att_unit)){
    for(j in 1:length(def_unit)){
      res[i,j] <- combat_round(att_unit[i],def_unit[j])
    }
  }
  return(res)
}

print(result2())

result1 <- function(){
  res<-sapply(1:10, function(x) {
  sapply(1:10, function(y) {
    combat_round(x,y,1)
    })
  })
  return(res)
}
# Print the result
print(result)


result1()

microbenchmark::microbenchmark(result2(),result1())




print(combat_round(def_units=1,att_units=2,sim=10000))


