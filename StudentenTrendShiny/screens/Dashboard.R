DashboardUI <- function(PageName) {
  return(
    tabItem(tabName = PageName,
      fluidRow(
        box(plotOutput("Dashboard", height = 250)),
        box(title = "Controls", sliderInput("slider", "Number of observations:", 1, 100, 50))
      )
    )
  )
}

DashboardServer <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$Dashboard <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}