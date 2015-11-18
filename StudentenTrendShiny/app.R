source("utils/prepare.R")

#Build the UI sections
ui <- dashboardPage(
   dashboardHeader(title = "StudentenTrend")
  ,dashboardSidebar(BuildSideBarMenu())
  ,dashboardBody(LoadApplicationBody())
)

#Call the Server functions
server <- function(input, output) {
  # Get all the pages that are currently in this application
  Pages <- GetPages()
  
  #Loop throught those pages and call the Server function for it
  for(Page in Pages) {
    FunctionName <- paste(Page["tabName"], "Server", sep="")
    
    #Try to call the server function, if not exist log it
    tryCatch({
      do.call(FunctionName, list(input, output) ) 
    }
    ,error = function(cond) {
      message(paste("Could not find the Server function of: ", Page["tabName"]))
    })
  }
}

#Run the application
shinyApp(ui, server)