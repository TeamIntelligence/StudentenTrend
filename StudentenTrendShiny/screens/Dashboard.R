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

DashboardServer <- function(input, output) {
  output$Dashboard_introText <- renderUI({
    HTML("StudentenTrend is een applicatie die inzichtelijk maakt wat er omtrent studenten en vacatures binnen Nederland gebeurd. 
      Deze applicatie bevat diverse grafieken die informatie  geven over bijvoorbeeld het aantal afgestudeerde studenten per studiesector 
      binnen Nederland. Ook is duidelijk te zien hoeveel studenten er staan ingeschreven en hoeveel de opleiding ook daadwerkelijk voltooien.
      Om wat aan deze data te hebben is ook het aantal vacatures nodig. Hiervoor hebben wij ook de data beschikbaar van het aantal banen per 
      sector binnen Nederland. <br /><br />
      Deze applicatie bevat ook voorspellingen voor het aantal banen, ingeschreven studenten en afgestudeerde studenten. Hiervoor gebruiken we 
      bestaande data en doen hier voorspellingen mee. <br /><br />
         
      Veel Succes!"
    )
  })
}