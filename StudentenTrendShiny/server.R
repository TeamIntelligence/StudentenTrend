# Call the Server functions
shinyServer(function(input, output) {
  # Get all the pages that are currently in this application
  Pages <- GetPages()
  
  # Loop through those pages and call the Server function for it
  for(Page in Pages) {
    # Loop through the children and try to find SubMenuItems
    for(i in 1:length(Page)) {
      SubItem <- Page[i]
      # Determine if the SubItem is really a SubItem 
      if(!is.null(SubItem) && names(SubItem) == "" && class(SubItem[[1]]) != "character") {
        # Fill a list object and call the server function
        SubItem <- list(tabName=GetPageNameSubItem(SubItem))
        CallServerFunction(Page=SubItem, input, output)
      }
    }
    
    CallServerFunction(Page=Page, input, output)
  }
})