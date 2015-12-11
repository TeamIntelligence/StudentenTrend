# Load our files
source("FileLoader.R")

# Build the UI sections
ui <- dashboardPage(
  dashboardHeader(title = "StudentenTrend")
  ,dashboardSidebar(BuildSideBarMenu())
  ,dashboardBody(LoadApplicationBody()),
  #Custom CSS for box titles
  tags$head(tags$style(HTML('
          .box-title {
            font-weight: bold;
            font-size: 24px;
          }
        ')))
)