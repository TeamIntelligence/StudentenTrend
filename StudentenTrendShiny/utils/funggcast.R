funggcast<-function(dn,fcast){ 
  require(zoo) #needed for the 'as.yearmon()' function
  en<-max(time(fcast$mean)) #extract the max date used in the forecast
  
  #Extract Source and Training Data
  ds<-as.data.frame(window(dn,end=en))
  names(ds)<-'observed'
  ds$date<-as.Date(time(window(dn,end=en)))
  
  #Extract the Fitted Values (need to figure out how to grab confidence intervals)
  dfit<-as.data.frame(fcast$fitted)
  dfit$date<-as.Date(time(fcast$fitted))
  names(dfit)[1]<-'fitted'
  
  ds<-merge(ds,dfit,all.x=T) #Merge fitted values with source and training data
  
  #Exract the Forecast values and confidence intervals
  dfcastn<-as.data.frame(fcast)
  dfcastn$date<-as.Date(as.yearmon(paste(row.names(dfcastn), "-01-01", sep = "")))
  names(dfcastn)<-c('fitted','lo80','hi80','lo95','hi95','date')
  
  pd<-merge(ds,dfcastn, all.x=T, all.y = T) #final data.frame for use in ggplot
  return(pd)
}