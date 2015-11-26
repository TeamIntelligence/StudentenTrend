GetPageNameSubItem <- function(SubItem) {
  succeeded <- FALSE
  
  # Try to get the SubItemName 
  tryCatch({
    succeeded <- TRUE
    return(SubItem[[1]]["children"][["children"]][[1]]["attribs"][[1]][["data-value"]])
  }
  ,error = function(cond) {})
  
  if(!succeeded) {
    return("")
  }
}

CallUIFunction <- function(Page) {
  FunctionName <- paste(Page["tabName"], "UI", sep="")
  
  #Try to call the UI function, if not exist log it
  tryCatch({
    Item <- do.call(FunctionName, list(Page["tabName"]) ) 
  }
  ,error = function(cond) {
    Item <<- tabItem(tabName = Page["tabName"],
                     fluidRow(
                       box(width=12, height = 800, title = paste("De ", Page["tabName"], " pagina is niet gevonden!")
                       )
                     )
    )
    
    message(paste("Could not find the UI function of: ", Page["tabName"]))
  })
  
  if(is.null(Item)) {
    Item <- get("Item", envir = .GlobalEnv)
    remove("Item", envir = .GlobalEnv)
  }
  
  return(Item)
}

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