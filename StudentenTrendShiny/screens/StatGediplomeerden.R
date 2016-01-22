StatGediplomeerdenUI <- function(PageName){
  return(
    tabItem(tabName = PageName,
            # Page title
            fluidRow(
              box(width=12, collapsible = T, title = "Statistische analyse van de gediplomeerden studenten", 
                  p("Op deze pagina vindt u een statistische analyse voor de ingeschreven studenten, de gediplomeerde studenten en voor de vervulde banen. De statistische analyse die uitgevoerd is, is de chikwadraattoets. Binnen de chikwadraattoets is gekozen voor een onafhankelijkheidstoets."),
                  p("De onafhankelijkheidstoets van chikwadraat, toetst of twee gegeven indelingen onafhankelijk zijn van elkaar. Als de uitkomst van deze toets een te grote waarde oplevert, mag er geconcludeerd worden dat er geen onafhankelijkheid is tussen te twee gegeven indelingen.")
              ),
              
              tabBox(width=12, height=550, 
                     tabPanel("Studieniveau",
                              h1("Chikwadraat"),
                              p("Zoals u in onderstaande grafiek kunt zien, is het verschil in het aantal gediplomeerde studenten voor HBO Bachelor en WO Bachelor voor sommige studiesectoren erg groot. Daarom is er het vermoeden dat er tussen HBO Bachelor en WO Bachelor studenten geen onafhankelijkheid is. Na het toetsen met chikwadraat, wordt dit vermoeden bevestigd."),
                              plotOutput("Gediplomeeren_HBOB_WOB", height = 450)
                     ),
                     tabPanel("Geslacht",
                              h1("Chikwadraat"),
                              p("Zoals u in onderstaande grafiek kunt zien, is het verschil in het aantal gediplomeerde mannelijke en vrouwelijke studenten voor sommige studiesectoren erg groot. Daarom is er het vermoeden dat er tussen mannelijke en vrouwelijke studenten geen onafhankelijkheid is. Na het toetsen met chikwadraat, wordt dit vermoeden bevestigd. "),
                              plotOutput("Gediplomeeren_Man_Vrouw", height = 450)      
                     ) 
              )
            )
    )   
    
  )
}

StatGediplomeerdenServer <- function(input,output, session){
  
  output$Gediplomeeren_HBOB_WOB <- renderPlot({
    DataDiploStudie<-read.csv("data/HO gediplomeerden analyse HBO WO csv.csv",header = T, sep = ";")
    
    ggplot(DataDiploStudie, aes(x=Studiesector, group = Studieniveau, colour = Studieniveau))+
      xlab("Studiesector") + 
      ylab("Percentage van het aantal studenten ten opzichte van het totaal aantal studenten per studieniveau") +
      ggtitle("Percentage van het aantal gediplomeerden in 2013 per studieniveau versus studiesector") +
      geom_line(aes(y=Percentage))+
      geom_point(aes(y=Percentage))+
      theme(axis.text.x=element_text(angle=-45, hjust = -0.005))
  })
  
  output$Gediplomeeren_Man_Vrouw <- renderPlot({
    DataDiploGeslacht<- read.csv("data/HO gediplomeerden man vrouw analyse csv.csv",header = T, sep = ";")
    ggplot(DataDiploGeslacht, aes(x=Studiesector, group = Geslacht, colour = Geslacht))+
      xlab("Studiesector") + 
      ylab("Percentage van het aantal studenten ten opzichte van het totaal aantal studenten per studieniveau") +
      ggtitle("Percentage van het aantal gediplomeerden in 2013 per studieniveau versus studiesector") +
      geom_line(aes(y=Percentage))+
      geom_point(aes(y=Percentage))+
      theme(axis.text.x=element_text(angle=-45, hjust = -0.005))
  })
  
}