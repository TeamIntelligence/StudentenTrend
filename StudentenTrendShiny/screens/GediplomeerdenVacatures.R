GediplomeerdenVacaturesUI <- function(PageName){
  return(
    
    tabItem(tabName = PageName,
            fluidRow(
              # Application title
              titlePanel("Aantal vacatures versus aantal gediplomeerden"),
              
              box(width=5, height = 470, plotOutput("VacaDiploPlot", height = 450))
            )  
            
    )
  )
}