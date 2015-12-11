GediplomeerdenVacaturesUI <- function(PageName){
  return(
    
    tabItem(tabName = PageName,
            # Page title
            titlePanel("Vervulde banen per studiesector") #,
#             fluidRow(
#               box(width=5, height = 150, 
#                   
#                   selectInput("GediplomeerdenVacatures_SelectImp",
#                               "Selecteer een of meerdere bedrijfssectoren om weer te geven:",
#                               choices = gediplomeerden_vacatures$sbiCode93.sbiNaam93,
#                               multiple = FALSE,
#                               selectize = TRUE,
#                               selected=1 
#                   )
#                   
#               ),
#               box(width=7, height=150
#              #slider
#                   
#               )
#               ,box(width=5, height = 470, plotOutput("GedipVacaPlot", height = 450))
#               ,box(width=7, height = 470, plotOutput("GedipVacaBarPlot", height=450))
#             )   
    )
  )
}

GediplomeerdenVacaturesServer <- function(input, output, session){
  
  
  
  
}