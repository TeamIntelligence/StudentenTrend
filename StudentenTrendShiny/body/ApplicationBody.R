#Load the whole application body section dynamically
LoadApplicationBody <- function() {
  # Get all the pages that are currently in this application
  Pages <- GetPages()
  TabItems <- NULL
  
  #Loop throught those pages and call the UI function for it
  for(Page in Pages) {
    FunctionName <- paste(Page["tabName"], "UI", sep="")
    Item         <- NULL
    
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
    
    #If the UI function is found add it to the layout
    if(!is.null(Item)) {
      if(is.null(TabItems)) {
        TabItems <- list(Item)
      } else {
        TabItems[[length(TabItems)+1]] <- Item
      }
    }
  }
  
  return(do.call(tabItems, TabItems) )
}