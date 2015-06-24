rankall<-function(n,num="best"){
  fulist<-read.csv("/home/chakri/Desktop/coursera/R Projects/R-Assignment/Assign-3/task2.csv",header=TRUE)
  
  #check if the state and outcomes are valid
  checkoutcome <- c("heart attack", "heart failure", "pneumonia")
  if ((n %in% checkoutcome) == FALSE) {
    stop(print("invalid outcome"))
  }
  #here ends its code
  
  st<-as.character(fulist$State)
  states<-matrix(st)
  ht<-as.character(fulist$Hospital.Name)
  hospitals<-matrix(ht)
  mt<-NULL
  failure<-NULL
  if(n=="heart failure"){
    mt<-as.numeric(fulist$death_failure)
    failure<-matrix(mt)}
  if(n=="heart attack")
  {
    mt<-as.numeric(fulist$death_attack)
    failure<-matrix(mt)
  }
  if(n=="pneumonia")
  {
    mt<-as.numeric(fulist$death_pne)
    failure<-matrix(mt)
  }
  uni<-states[!duplicated(states)]
  uniques<-matrix(sort(uni))
  
  for(i in 1:length(uniques))
  {
     lk1<-which(states==uniques[i],arr.ind = FALSE)
      statesfil<-states[lk1]
      hospitalsfil<-hospitals[lk1]
      failurefil<-failure[lk1]
      
      newlk<-sort(hospitalsfil,index.return=TRUE)
      statesfil<-statesfil[newlk$ix]
      hospitalsfil<-hospitalsfil[newlk$ix]
      failurefil<-failurefil[newlk$ix]
      
      lk2<-which(!is.na(failurefil),arr.ind = FALSE)
      statesfil2<-statesfil[lk2]
      hospitalsfil2<-hospitalsfil[lk2]
      failurefil2<-failurefil[lk2]
      
      lk3<-sort(failurefil2,index.return = TRUE)
      finalstate<-statesfil2[lk3$ix]
      finalhospital<-hospitalsfil2[lk3$ix]
      finalfailure<-failurefil2[lk3$ix]
      
      if(num=="best")
      {
        cat(sprintf("%s\t%s\n",finalhospital[1],finalstate[1]))
        
      }
      if(num=="worst")
      {
        cat(sprintf("%s\t%s\n",finalhospital[length(finalhospital)],finalstate[length(finalhospital)]))
       
      }
      else{
        cat(sprintf("%s\t%s\n",finalhospital[num],finalstate[num]))
                }
  }
}

rankall("heart attack", 4)
#for checking the error conditions, remove the above function snippet and remove the below if case with
#block inside as it is (just remove these if(FALSE), { , })

if(FALSE)
{
r<- tryCatch({
  rankall("heart falure", 10)
}, error = function(e) {
  e
})
if(!inherits(r, "error"))
  stop("'rankhospital' should throw an error via 'stop' in this case")
tolower(conditionMessage(r))
}



