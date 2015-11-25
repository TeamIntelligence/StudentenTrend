CallServerFunction <- function(Page, ...) {
  FunctionName <- paste(Page["tabName"], "Server", sep="")
  
  #Try to call the server function, if not exist log it
  tryCatch({
    do.call(FunctionName, list(...) ) 
  }
  ,error = function(cond) {
    message(paste("Could not find the Server function of: ", Page["tabName"]))
  })
}