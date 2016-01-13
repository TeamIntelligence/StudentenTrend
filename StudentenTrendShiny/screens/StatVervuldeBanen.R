StatVervuldeBanenUI <- function(PageName){
  return(
    tabItem(tabName = PageName,
            # Page title
            fluidRow(
              box(width=12, collapsible = T, title = "Statistische analyse van de vervulde banen", 
                  p("Op deze pagina vindt u een statistische analyse voor de ingeschreven studenten, de gediplomeerde studenten en voor de vervulde banen. De statistische analyse die uitgevoerd is, is de chikwadraattoets. Binnen de chikwadraattoets is gekozen voor een onafhankelijkheidstoets."),
                  p("De onafhankelijkheidstoets van chikwadraat, toetst of twee gegeven indelingen onafhankelijk zijn van elkaar. Als de uitkomst van deze toets een te grote waarde oplevert, mag er geconcludeerd worden dat er geen onafhankelijkheid is tussen te twee gegeven indelingen.")
              ),
              
              tabBox(width=12, height=550, 
                     tabPanel("HBO",
                              h1("Chikwadraat"),
                              p("Zoals u in onderstaande grafiek kunt zien, is het verschil in het aantal studenten dat binnen drie jaar aan een baan komt per studiesector voor sommige bedrijfssectoren erg groot. Daarom is er het vermoeden dat er tussen de verschillende studiesectoren geen onafhankelijkheid is. Na het toetsen met chikwadraat, wordt dit vermoeden bevestigd. "),
                              plotOutput("VervuldeBanen_HBO", height = 450)
                     ),
                     tabPanel("WO",
                              h1("Chikwadraat"),
                              p("Zoals u in onderstaande grafiek kunt zien, is het verschil in het aantal studenten dat binnen drie jaar aan een baan komt per studiesector voor sommige bedrijfssectoren erg groot. Daarom is er het vermoeden dat er tussen de verschillende studiesectoren geen onafhankelijkheid is. Na het toetsen met chikwadraat, wordt dit vermoeden bevestigd. "),
                              plotOutput("VervuldeBanen_WO", height = 450)      
                     ) 
              )
            )
    )   
    
  )
}

StatVervuldeBanenServer <- function(input,output, session){
  
  
  output$VervuldeBanen_HBO <- renderPlot({
    dataHBO <- read.csv("data/Studenten_met_baan_HBO_2005.csv", sep = ";")
    ggplot(data = dataHBO, aes(x = Bedrijfssector)) +
      xlab("Bedrijfssector") +
      ylab("Aantal") +
      ggtitle("Aantal HBO gediplomeerden in 2005 per studiesector versus de bedrijfssector waarin ze binnen drie jaar aan een baan komen") +
      geom_line(data = dataHBO, aes(y = Aantal,   
                                    group = Studiesector,
                                    color = Studiesector)) + 
      geom_point(data = dataHBO,aes(y = Aantal, 
                                    group = Studiesector,
                                    color = Studiesector))+ 
      theme(axis.text.x=element_text(angle=-45, hjust = -0.005))
  })  
  
  output$VervuldeBanen_WO <- renderPlot({
    dataWO <- read.csv("data/Studenten_met_baan_WO_2005.csv", sep = ";")
    ggplot(data = dataWO, aes(x = Bedrijfssector)) +
      xlab("Bedrijfssector") +
      ylab("Aantal") +
      ggtitle("Aantal WO gediplomeerden in 2005 per studiesector versus de bedrijfssector waarin ze binnen drie jaar aan een baan komen") +
      geom_line(data = dataWO, aes(y = Aantal,   
                                   group = Studiesector,
                                   color = Studiesector)) + 
      geom_point(data = dataWO,aes(y = Aantal, 
                                   group = Studiesector,
                                   color = Studiesector)) +
      theme(axis.text.x=element_text(angle=-45, hjust = -0.005))
  })
  
  

}