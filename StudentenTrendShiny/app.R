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
    for(SubItem in Page["subItems"]) {
      CallServerFunction(Page=SubItem, input, output)
    }
    
    CallServerFunction(Page=Page, input, output)
  }
}

#Run the application
shinyApp(ui, server)