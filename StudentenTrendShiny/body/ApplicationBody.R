# Load the whole application body section dynamically
LoadApplicationBody <- function() {
  # Get all the pages that are currently in this application
  Pages <- GetPages()
  TabItems <- NULL
  
  # Loop throught those pages and call the UI function for it
  for(Page in Pages) {
    Item         <- CallUIFunction(Page)
    NewList      <- NULL
    
    # If the UI function is found add it to the layout
    if(!is.null(Item)) {
      if(is.null(TabItems)) {
        TabItems <- list(Item)
        NewList  <- TRUE
      } else {
        TabItems[[length(TabItems)+1]] <- Item
        NewList  <- FALSE
      }
    }
    
    for(i in 1:length(Page)) {
      SubItem <- Page[i]
      if(!is.null(SubItem) && names(SubItem) == "" && class(SubItem[[1]]) != "character") {
        SubItem <- list(tabName=GetPageNameSubItem(SubItem))
        Item    <- CallUIFunction(Page=SubItem)
        
        TabItems[[length(TabItems)+1]] <- Item
      }
    }
  }
  
  return(do.call(tabItems, TabItems) )
}