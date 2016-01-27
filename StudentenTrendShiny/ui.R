# Load our files
source("FileLoader.R")

# Build the UI sections
ui <- dashboardPage(
  dashboardHeader(title = "StudentenTrend")
  ,dashboardSidebar(BuildSideBarMenu())
  ,dashboardBody(LoadApplicationBody(), 
                 includeCSS("resources/custom_plotly.css"),
                 tags$head(tags$script(src="underscore-min.js")),
                 tags$head(tags$script(src="plotly_legend.js")))
  #Custom CSS for box titles
  ,tags$head(tags$style(HTML('
          .box-title {
            font-weight: bold;
            font-size: 24px;
          }
        ')))
)