GetPageNameSubItem <- function(SubItem) {
  
  # Try to get the SubItemName 
  tryCatch({
    if(!is.null(SubItem[[1]]["children"][["children"]][[1]]["attribs"][[1]][["data-value"]])) {
      return(SubItem[[1]]["children"][["children"]][[1]]["attribs"][[1]][["data-value"]])
    }
  }
  ,error = function(cond) {}
  )
  
  return("")
}

CallUIFunction <- function(Page) {
  if(!is.null(Page[["tabName"]])) {
    FunctionName <- paste(Page["tabName"], "UI", sep="")
    
    #Try to call the UI function, if not exist log it
    if(exists(FunctionName, envir=.GlobalEnv)) {
      Item <- do.call(FunctionName, list(Page["tabName"]) ) 
    } else {
      Item <- tabItem(tabName = Page["tabName"],
                      fluidRow(
                        box(width=12, height = 800, title = paste("De ", Page["tabName"], " pagina is niet gevonden!")
                        )
                      )
      )
      
      message(paste("Could not find the UI function of: ", Page["tabName"]))
    }
    
    return(Item)
  }
}

CallServerFunction <- function(Page, ...) {
  if(!is.null(Page[["tabName"]]) ) {
    FunctionName <- paste(Page["tabName"], "Server", sep="")
    
    #Try to call the server function, if not exist log it
    if(exists(FunctionName, envir=.GlobalEnv)) {
      do.call(FunctionName, list(...) ) 
    } else {
      message(paste("Could not find the Server function of: ", Page["tabName"]))
    }
  }
  
  Test <<- Page
}