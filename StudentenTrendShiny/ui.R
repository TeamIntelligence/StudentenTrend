source("prepare.R")

shinyUI(fluidPage(
  
  # Application title
  titlePanel("StudentenTrend"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition="input.conditionedPanels==1 || input.conditionedPanels==2",
                       selectInput("selectStudy",
                                   "Selecteer een of meerdere studiesectoren om weer te geven:",
                                   choices = studievoortgang$iscedCode$iscedNaam,
                                   multiple = TRUE,
                                   selectize = TRUE,
                                   selected = 1
                                   ),
                       helpText("Je kan ook typen om te zoeken")
                       ),
      conditionalPanel(condition   ="input.conditionedPanels==3",
                       radioButtons("voortgangType",
                                    "Soort", 
                                    choices = list("Uitschrijf percentages" = "uitschrijf", 
                                                   "Afstudeer percentages"  = "afgestudeerd")
                                    ),
                       uiOutput    ("yearRange"),
                       helpText    ("Gemiddelde afstudeer/Uitschrijf percentages per studie binnen ... jaar na aanvang")
                       ) 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Aantal Studenten per opleidings-sector", value=1, plotOutput("aantalStudentenPlot", height = "800px"), uiOutput("panel1Help")), 
        tabPanel("Panel 2", value=2, plotOutput("hboWoverhouding")),
        tabPanel("Haalbaarheid", value=3, plotOutput("voortgangsPercentages", height = "800px", width = "auto"))
        , id = "conditionedPanels"
      )
    )
  )
))