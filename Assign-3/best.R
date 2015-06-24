best<-function(stato,n)
{
liste<-read.csv("/home/chakri/Desktop/coursera/R Projects/R-Assignment/Assign-3/outcome.csv",header=TRUE)
#check if the state and outcomes are valid
xe<-liste$State
checkstate <- matrix(xe)
checkoutcome <- c("heart attack", "heart failure", "pneumonia")
if ((stato %in% checkstate) == FALSE) {
  stop(print("invalid state"))
}
else if ((n %in% checkoutcome) == FALSE) {
  stop(print("invalid outcome"))
}
#here ends its code
st<-as.character(liste$State)
states<-matrix(c(st))
hs<-as.character(liste$Hospital.Name)
hospital<-matrix(c(hs))
cs<-NULL
cspec<-NULL
if(n=="heart attack")
{
  spe<-"lmr_hattack"
  cs<-as.numeric(liste$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  cspec<-matrix(c(cs))
}
if(n=="heart failure")
{
  spe<-"lmr_hfailure"
  cs<-as.numeric(liste$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  cspec<-matrix(c(cs))
}
if(n=="pneumonia")
{
  spe<-"lmr_pneumonia"
  cs<-as.numeric(liste$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  cspec<-matrix(c(cs))
}
head(cspec)
lk1<-which(states==stato,arr.ind = FALSE)
statesfil<-states[lk1]
hospitalsfil<-hospital[lk1]
failurefil<-cspec[lk1]

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

return(finalhospital[1])
}
best("NY", "pneumonia")

#for checking the error conditions, remove the above function snippet and remove the below if case with
#block inside as it is (just remove these if(FALSE), { , })

if(FALSE)
{
r <- tryCatch(best("NN", "pneumonia"), error = function(e) e)
if(!inherits(r, "error"))
  stop("'best' should throw an error via the 'stop' function in this case")
tolower(conditionMessage(r))
}