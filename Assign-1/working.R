g<-function(z,pollutant,x,y=0){
files_full <- list.files(paste("/home/chakri/Desktop",z,sep="/"), full.names=TRUE)
dat <- data.frame()
if(y==0)
{
  dat<-read.csv(files_full[x])
}
else{
for (i in x:y)
{
  print(files_full[i])
  dat <- rbind(dat, read.csv(files_full[i]))  
}}
if(pollutant=="sulfate"){val<-dat$sulfate}
else{val<-dat$nitrate}
sulfate_mean<-mean(val,na.rm=TRUE)
sulfate_mean
}
g("specdata","nitrate",70,80)