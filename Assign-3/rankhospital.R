rankhospital<-function(sta,n,num){
  fulist<-read.csv("/home/chakri/Desktop/coursera/R Projects/R-Assignment/Assign-3/task2.csv",header=TRUE)
  
  #check if the state and outcomes are valid
  xe<-fulist$State
  checkstate <- matrix(xe)
  checkoutcome <- c("heart attack", "heart failure", "pneumonia")
  if ((sta%in% checkstate) == FALSE) {
    stop(print("invalid state"))
  }
  else if ((n %in% checkoutcome) == FALSE) {
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
  
  lk1<-which(states==sta,arr.ind = FALSE)
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
  #df<-data.frame('Hospital'=finalhospital,'Rate'=finalfailure)
  #write.table(df,sep=" ",eol = "\n")
  cat(sprintf("Rank\tRate\t\t\tHospital\n\n"))
  for(i in 1:length(finalhospital))
  {
    cat(sprintf("%d\t%-10.1f\t%s\t%s\n",i,finalfailure[i],finalhospital[i],finalstate[i]))
  }
 
  if(num=="best")
  {
    return(finalhospital[1])
  }
  else if(num=="worst")
  {
    return(finalhospital[length(finalhospital)])
  }
  else if(num>length(finalhospital))
  {
    return("NA")
  }
  return(finalhospital[num])
}

rankhospital("NC", "heart attack", "worst")

#for checking the error conditions, remove the above function snippet and remove the below if case with
#block inside as it is (just remove these if(FALSE), { , })
if(FALSE)
{
  r <- tryCatch({
    rankhospital("NY", "heart attak", 7)
  }, error = function(e) {
    e
  })
  if(!inherits(r, "error"))
    stop("'rankhospital' should throw an error via 'stop' in this case")
  tolower(conditionMessage(r))
}