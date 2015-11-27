# Load our files
source("FileLoader.R")

# Build the UI sections
ui <- dashboardPage(
  dashboardHeader(title = "StudentenTrend")
  ,dashboardSidebar(BuildSideBarMenu())
  ,dashboardBody(LoadApplicationBody())
)