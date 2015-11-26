GediplomeerdenVacaturesUI <- function(PageName){
  return(
    
    tabItem(tabName = PageName,
      # Page title
      titlePanel("Aantal vacatures versus aantal gediplomeerden"),
      
      fluidRow(
        box(width=5, height = 470, plotOutput("VacaDiploPlot", height = 450))
      )  
    )
  )
}