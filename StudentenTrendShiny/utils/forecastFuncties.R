createForecastSub <- function(INPUTSET, GROUPBY, START, END, EXCLUDE){
  
  if(GROUPBY == "totaal"){
    timeSeries   <- ts(INPUTSET$aantal, start=START, end=END, frequency=1)
    
    tryCatch({
      fits         <- Arima(timeSeries, order = c(2,0,0),method="CSS")
    }
    ,error = function(cond) {
      tryCatch({
        fits         <<- Arima(timeSeries, order = c(2,0,0),method="ML")
      }
      ,error = function(cond) {
          fits         <<- auto.arima(timeSeries)
      })
    })
    
    if(is.null(fits)) {
      fits <- get("fits", envir = .GlobalEnv)
      remove("fits", envir = .GlobalEnv)
    }
    
    forecastData <- funggcast(timeSeries, forecast(fits))
    forecastData <- subset(forecastData, is.na(forecastData$observed))
  } else {
    timeSeries <- tapply(INPUTSET$aantal, INPUTSET[GROUPBY], ts, start=START, end=END, frequency=1)
    
    tryCatch({
      fits         <- lapply(timeSeries, Arima, order=c(2,0,0),method="CSS")
    }
    ,error = function(cond) {
      tryCatch({
        fits         <<- lapply(timeSeries, Arima, order=c(2,0,0),method="ML")
      }
      ,error = function(cond) {
          fits         <<- lapply(timeSeries, auto.arima)
      })
    })
    
    if(is.null(fits)) {
      fits <- get("fits", envir = .GlobalEnv)
      remove("fits", envir = .GlobalEnv)
    }
  
    forecastData <- list()
    for(name in names(timeSeries)){
      forecastData[[name]] <- funggcast(timeSeries[[name]], forecast(fits[[name]]))
      forecastData[[name]] <- subset(forecastData[[name]],is.na(forecastData[[name]]$observed))
    }
    
  }
  
  mergedSub <- mergeForecastframe(INPUTSET, forecastData, GROUPBY, EXCLUDE)
}

funggcast <- function(dn,fcast){ 
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

mergeForecastframe <- function(normalSet, forecastSet, sbiOrIsced, excludeYear){
  if(sbiOrIsced == "totaal"){
    newDf <- data.frame(jaartal=NA, fitted=NA, lo80=NA, hi80=NA, lo95=NA, hi95=NA)
    
    tempDf <- forecastSet
    tempDf[["date"]] <- format(tempDf[["date"]], "%Y")
    names(tempDf)[names(tempDf)=="date"] <- "jaartal"
    # names(tempDf)[names(tempDf)=="fitted"] <- "aantal"
    newDf <- merge(newDf, tempDf, all=T)
    
  } else {
    newDf <- data.frame(a=NA, jaartal=NA, fitted=NA, lo80=NA, hi80=NA, lo95=NA, hi95=NA)
    colnames(newDf)[1] <- sbiOrIsced
    
    for(name in names(forecastSet)){
      tempDf <- forecastSet[[name]]
      if (sbiOrIsced != "ignore"){
        tempDf[[sbiOrIsced]] <- c(name)
      }
      tempDf[["date"]] <- format(tempDf[["date"]], "%Y")
      names(tempDf)[names(tempDf)=="date"] <- "jaartal"
      # names(tempDf)[names(tempDf)=="fitted"] <- "aantal"
      newDf <- merge(newDf, tempDf, all=T)
    }
  }
  
  newDf <- newDf[rowSums(is.na(newDf)) != ncol(newDf),]
  
  normalSet <- normalSet[normalSet$jaartal != excludeYear, ]
  mergedSet <- merge(normalSet, newDf, all = T)

  return(mergedSet)
}