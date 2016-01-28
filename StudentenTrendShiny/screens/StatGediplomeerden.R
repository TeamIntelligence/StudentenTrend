StatGediplomeerdenUI <- function(PageName){
  return(
    tabItem(tabName = PageName,
            # Page title
            fluidRow(
              box(width=12, collapsible = T, title = "Statistische analyse van de gediplomeerden studenten", 
                  p("Op deze pagina vindt u een statistische analyse voor de ingeschreven studenten, de gediplomeerde studenten en voor de vervulde banen. De statistische analyse die uitgevoerd is, is de chikwadraattoets. Binnen de chikwadraattoets is gekozen voor een onafhankelijkheidstoets."),
                  p("De onafhankelijkheidstoets van chikwadraat, toetst of twee gegeven indelingen onafhankelijk zijn van elkaar. Als de uitkomst van deze toets een te grote waarde oplevert, mag er geconcludeerd worden dat er geen onafhankelijkheid is tussen te twee gegeven indelingen.")
              ),
              
              tabBox(width=12, height=650, 
                     tabPanel("Studieniveau",
                              h1("Chikwadraat"),
                              p("Zoals u in onderstaande grafiek kunt zien, is het verschil in het aantal gediplomeerde studenten per studieniveau voor sommige studiesectoren erg groot. Daarom is er het vermoeden dat er tussen studieniveau en studiesector geen onafhankelijkheid is. Na het toetsen met chikwadraat, wordt dit vermoeden bevestigd."),
                              p("Voor de studiesector natuurwetenschappen is het aantal studenten onafhankelijk van de studieniveaus HBO bachelor en WO bachelor."),
                              p("Eveneens geldt voor de studiesector techniek dat het aantal studenten onafhankelijk is van de studieniveaus WO bachelor en WO master."),
                              plotlyOutput("Gediplomeeren_HBOB_WOB", height = 450)
                     ),
                     tabPanel("Geslacht",
                              h1("Chikwadraat"),
                              p("Zoals u in onderstaande grafiek kunt zien, is het verschil in het aantal gediplomeerde mannelijke en vrouwelijke studenten voor sommige studiesectoren erg groot. Daarom is er het vermoeden dat er tussen mannelijke en vrouwelijke studenten geen onafhankelijkheid is. Na het toetsen met chikwadraat, wordt dit vermoeden bevestigd. "),
                              p("Er zijn geen onderlinge onafhankelijkheden gevonden voor geslacht."),
                              plotlyOutput("Gediplomeeren_Man_Vrouw", height = 450)      
                     ) 
              )
            )
    )   
    
  )
}

StatGediplomeerdenServer <- function(input,output, session){
  
  output$Gediplomeeren_HBOB_WOB <- renderPlotly({
    DataDiploStudie<-read.csv("data/HO gediplomeerden analyse HBO WO csv.csv",header = T, sep = ";")
    
    MakeStatGGPlotly(
      ggplot(DataDiploStudie, aes(x=Studiesector, group = Studieniveau, colour = Studieniveau))+
        xlab("Studiesector") + 
        ylab("Percentage") +
        ggtitle("Percentage van het aantal gediplomeerden in 2013 per studieniveau versus studiesector") +
        geom_line(aes(y=Percentage), size=-1) +
        geom_point(aes(y=Percentage), size=-1) +
        theme(axis.text.x=element_text(angle=-45, hjust = -0.005)) +
        scale_color_manual(values=GetColors(DataDiploStudie$Studieniveau), labels=unique(DataDiploStudie$Studieniveau))
    )
  })
  
  output$Gediplomeeren_Man_Vrouw <- renderPlotly({
    DataDiploGeslacht<- read.csv("data/HO gediplomeerden man vrouw analyse csv.csv",header = T, sep = ";")
    
    MakeStatGGPlotly(
      ggplot(DataDiploGeslacht, aes(x=Studiesector, group = Geslacht, colour = Geslacht))+
        xlab("Studiesector") + 
        ylab("Percentage") +
        ggtitle("Percentage van het aantal gediplomeerden in 2013 per studieniveau versus studiesector") +
        geom_line(aes(y=Percentage), size=-1) +
        geom_point(aes(y=Percentage), size=-1) +
        theme(axis.text.x=element_text(angle=-45, hjust = -0.005)) +
        scale_color_manual(values=GetColors(DataDiploGeslacht$Geslacht), labels=unique(DataDiploGeslacht$Geslacht) )
    )
  })
}