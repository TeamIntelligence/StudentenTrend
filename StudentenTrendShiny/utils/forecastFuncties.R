createForecastSub <- function(INPUTSET, COUNTCOL, GROUPBY, START, END, EXCLUDE,DF=2){
  if(GROUPBY == "singleColumn"){
    timeSeries   <- ts(INPUTSET[[COUNTCOL]], start=START, end=END, frequency=1)
    
    tryCatch({
      fits         <- Arima(timeSeries, order = c(DF,0,0),method="CSS")
    }
    ,error = function(cond) {
      tryCatch({
        fits         <<- Arima(timeSeries, order = c(DF,0,0),method="ML")
      }
      ,error = function(cond) {
          fits         <<- auto.arima(timeSeries)
      })
    })
    
    if(is.null(fits)) {
      fits <- get("fits", envir = .GlobalEnv)
      remove("fits", envir = .GlobalEnv)
    }
    
    forecastData <- funggcast(timeSeries, forecast(fits,h=5),END+1, 5)
    forecastData <- subset(forecastData, is.na(forecastData$observed))
  } else {
    timeSeries <- tapply(INPUTSET[[COUNTCOL]], INPUTSET[GROUPBY], ts, start=START, end=END, frequency=1)
    
    fits <<- vector("list", length(timeSeries))
    names(fits) <<- names(timeSeries)
    
    i <- 1
    for(serie in timeSeries){
      tryCatch({
        fits[[i]] <<- Arima(serie, order=c(DF,0,0),method="CSS")
        # print("CHOSE CSS")
      }
      ,error = function(cond) {
        tryCatch({
          fits[[i]] <<- Arima(serie, order=c(DF,0,0), method="ML")
          # print("CHOSE ML")
        }
        ,error = function(cond) {
          fits[[i]] <<- auto.arima(serie)
          # print("CHOSE AUTO")
        })
      })
      i <- i + 1
    }
    # print("Completed arima's")
    
    if(is.null(fits)) {
      fits <- get("fits", envir = .GlobalEnv)
      remove("fits", envir = .GlobalEnv)
    }
    
    forecastData <- list()
    for(name in names(timeSeries)){
      forecastData[[name]] <- funggcast(timeSeries[[name]], forecast(fits[[name]], h=5), END+1, 5)
      forecastData[[name]] <- subset(forecastData[[name]],is.na(forecastData[[name]]$observed))
    }
    
  }
  
  mergedSub <- mergeForecastframe(INPUTSET, forecastData, GROUPBY, EXCLUDE)
}

funggcast <- function(dn,fcast,fitStart,fitDuration){ 
  require(zoo) #needed for the 'as.yearmon()' function
  en<-max(time(fcast$mean)) #extract the max date used in the forecast
  #Extract Source and Training Data
  ds<-as.data.frame(window(dn))
  
  names(ds)<-'observed'
  ds$date<-as.Date(time(window(dn)))

  #Extract the Fitted Values (need to figure out how to grab confidence intervals)
  dfit<-as.data.frame(fcast$fitted)
  dfit$date<-as.Date(time(fcast$fitted))
  names(dfit)[1]<-'fitted'
  
  ds<-merge(ds,dfit,all.x=T) #Merge fitted values with source and training data

  #Exract the Forecast values and confidence intervals
  dfcastn<-as.data.frame(fcast)
  
  if(row.names(dfcastn)[1] != fitStart){
    nameArr <- c()
    for(newName in fitStart:(fitStart+fitDuration-1)){
      nameArr <- c(nameArr, newName)
    }
    row.names(dfcastn) <- nameArr
  }
  
  dfcastn$date<-as.Date(as.yearmon(paste(row.names(dfcastn), "-01-01", sep = "")))

  names(dfcastn)<-c('fitted','lo80','hi80','lo95','hi95','date')
  
  for(rowName in row.names(dfcastn)){
    if(dfcastn[rowName, "fitted"] < 0){
      dfcastn[rowName, "fitted"] <- 0
    }
  }
  
  pd<-merge(ds,dfcastn, all.x=T, all.y = T) #final data.frame for use in ggplot
  return(pd)
}

mergeForecastframe <- function(normalSet, forecastSet, sbiOrIsced, excludeYear){
  if(sbiOrIsced == "singleColumn"){
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