DashboardUI <- function(PageName) {
  return(
    tabItem(tabName = PageName,
      titlePanel("Welkom bij StudentenTrend!"),
      fluidRow(
        box(width=12, height=500,
            htmlOutput("Dashboard_introText")
        )
      )
    )
  )
}

DashboardServer <- function(input, output, session) {
  output$Dashboard_introText <- renderUI({
    HTML("StudentenTrend is een applicatie die inzichtelijk maakt hoe het aantal studenten en vacatures binnen Nederland beweegt over de tijd. 
          Deze applicatie bevat diverse grafieken die informatie geven over bijvoorbeeld het aantal eerstejaars, ingeschreven en afgestudeerde 
          studenten per studiesector binnen Nederland. Daarnaast zijn er ook grafieken te vinden omtrent het aantal vacatures per bedrijfssector 
          en in welke mate er per studierichting banen vervuld worden door afgestudeerde studenten.
         <br /><br />
         De data die gebruikt is om deze grafieken mee te maken, is ook gebruikt om een aantal interessante voorspellingen te doen. 
         <br /><br />
         Veel succes!"
    )
  })
}