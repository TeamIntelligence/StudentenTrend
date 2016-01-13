StatIngeschrevenUI <- function(PageName){
  return(
    tabItem(tabName = PageName,
            # Page title
            fluidRow(
              box(width=12, collapsible = T, title = "Statistische analyse van de ingeschreven studenten", 
                  p("Op deze pagina vindt u een statistische analyse voor de ingeschreven studenten, de gediplomeerde studenten en voor de vervulde banen. De statistische analyse die uitgevoerd is, is de chikwadraattoets. Binnen de chikwadraattoets is gekozen voor een onafhankelijkheidstoets."),
                  p("De onafhankelijkheidstoets van chikwadraat, toetst of twee gegeven indelingen onafhankelijk zijn van elkaar. Als de uitkomst van deze toets een te grote waarde oplevert, mag er geconcludeerd worden dat er geen onafhankelijkheid is tussen te twee gegeven indelingen.")
              ),
              
              tabBox(width=12, height=550, 
                       tabPanel("HBO Bachelor en WO Bachelor",
                                h1("Chikwadraat"),
                                p("Zoals u in onderstaande grafiek kunt zien, is het verschil in het aantal ingeschreven studenten voor HBO Bachelor en WO Bachelor voor sommige studiesectoren erg groot. Daarom is er het vermoeden dat er tussen HBO Bachelor en WO Bachelor studenten geen onafhankelijkheid is. Na het toetsen met chikwadraat, wordt dit vermoeden bevestigd."),
                                plotOutput("Ingeschreven_HBOB_WOB", height = 450)
                        ),
                        tabPanel("Geslacht",
                                 h1("Chikwadraat"),
                                 p("Zoals u in onderstaande grafiek kunt zien, is het verschil in het aantal ingeschreven mannelijke en vrouwelijke studenten voor sommige studiesectoren erg groot. Daarom is er het vermoeden dat er tussen mannelijke en vrouwelijke studenten geen onafhankelijkheid is. Na het toetsen met chikwadraat, wordt dit vermoeden bevestigd. "),
                                 plotOutput("Ingeschreven_Man_Vrouw", height = 450)      
                        ) 
              )
            )
    )   
    
  )
}

StatIngeschrevenServer <- function(input,output, session){
  
  output$Ingeschreven_HBOB_WOB <- renderPlot({
    DataIngeschreven <-read.csv("data/HO ingeschreven analyse csv.csv",header = T, sep = ";")
    
    ggplot(DataIngeschreven, aes(x=Studiesector, group = Studieniveau, colour = Studieniveau))+
      xlab("Studiesector") + 
      ylab("Aantal studenten") +
      ggtitle("Aantal HBO en WO Bachelor ingeschrevenen in 2014 per studieniveau versus studiesector") +
      geom_line(aes(y=Aantal))+
      geom_point(aes(y=Aantal))+
      theme(axis.text.x=element_text(angle=-45, hjust = -0.005))
    
  })

  output$Ingeschreven_Man_Vrouw <- renderPlot({
    DataIngeschrevenManVrouw <- read.csv("data/HO ingeschreven man vrouw analyse csv.csv",header = T, sep = ";")

    ggplot(DataIngeschrevenManVrouw, aes(x=Studiesector, group = Geslacht, colour = Geslacht))+
      xlab("Studiesector") + 
      ylab("Aantal studenten") +
      ggtitle("Aantal man en vrouw ingeschrevenen in 2014 per studieniveau versus studiesector") +
      geom_line(aes(y=Aantal))+
      geom_point(aes(y=Aantal))+
      theme(axis.text.x=element_text(angle=-45, hjust = -0.005))
  }) 
    
}