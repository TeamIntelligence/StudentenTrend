#The UI function for the StudentenPerSector page
StudentenPerSectorUI <- function(PageName) {
  return(
    tabItem(tabName = PageName,
      fluidRow(
        box(width = 12, title = "Eerstejaarsstudenten",
            p("Op deze pagina vindt u het aantal eerstejaarsstudenten per studiesector over de periode 1995 tot en met 2012. HBO en WO is samengenomen. U kunt zelf kiezen welke studiesectoren u wilt weergeven. Daarnaast kunt u ook een totaallijn weergeven van alle studies of een totaallijn van de studies die u geselecteerd hebt."),
            p("De grafiek biedt inzicht hoeveel studenten elk jaar starten binnen een bepaalde studie sector. Er kan vervolgens uit opgemaakt worden of studies binnen een bepaalde studiesector groeien of afnemen."),
            collapsible = T
            ),
        box(width=6, height = 170,
            selectInput("StudentenPerSector_selectStudyImp",
                        "Selecteer een of meerdere studiesectoren om weer te geven:",
                        choices = studievoortgang$iscedCode.iscedNaam,
                        multiple = TRUE,
                        selectize = TRUE),
            checkboxInput("StudentenPerSector_AlleStudies",
                          "Geef alle studies weer"
            )
        ),
        box(width = 6, height = 170,
            checkboxInput("StudentenEerstejaars_Totaalselect",
                          "Totaal lijn weergeven van de geselecteerde studies"
            ),
            checkboxInput("StudentenEerstejaars_Totaal",
                          "Totaal lijn weergeven"
            )
        )
        # Show a plot of the generated distribution
        ,
        tabBox( width=12, height=550,
          tabPanel("Huidig", 
                   box(width=5, plotOutput("StudentenPerSector_aantalStudentenPlot", height=450)), 
                   box(width=7, plotOutput("StudentenPerSector_aantalStudentenBarPlot", height=450))
                   ),
          tabPanel("Voorspelling",
                   box(width=12,plotOutput("StudentenPerSector_aantalStudentenVoorspellingPlot", height = 450))
          )
        )
      )
    )
  )
}

#The Server function for the StudentenPerSector page
StudentenPerSectorServer <- function(input, output, session) {
  
  output$StudentenPerSector_aantalStudentenPlot <- renderPlot({
    
    svSub <- studievoortgang[studievoortgang$iscedCode.iscedNaam %in% input$StudentenPerSector_selectStudyImp,]
    
    PlotTitle <- "Aantal eerstejaarsstudenten \nper jaar verdeeld per studie"
    
#         ggplot(svSub, aes(x=svSub$jaartal, y=svSub$aantal), environment=environment()) +
#           xlab("Jaar") +
#           ylab("Aantal Studenten") +
#           geom_bar(stat = "identity", fill="red", alpha = 1/2)+
#           ggtitle(plotTitle)
#       
#         ggplot(svSub, aes(x=svSub$jaartal, y=svSub$aantal, fill=svSub$iscedCode$iscedNaam), environment=environment()) +
#           xlab("Jaar") +
#           ylab("Aantal Studenten") +
#           geom_bar(stat = "identity")+
#           ggtitle(plotTitle) +
#           scale_fill_manual(values=rainbow(length(input$StudentenPerSector_selectStudyImp)),name="Opleidings Sector")

        if (input$StudentenEerstejaars_Totaal == TRUE & input$StudentenEerstejaars_Totaalselect == TRUE ){ 
          ##allebei de lijnen
          ##select lijn
          totaalaantalselect <- TotaalAantalSelect(data =studievoortgang, 
                                                   selectInput = input$StudentenPerSector_selectStudyImp, 
                                                   filterParams= c("jaartal"))
 
          #Totaal berekenen
          totaalaantal <- TotaalAantal(data =studievoortgang, 
                                       filterParams= c("jaartal"))
          
          #plotten
          ggplot(svSub, aes(x=jaartal)) + 
            xlab("Jaar") +  
            ylab("Aantal studenten") + 
            ggtitle(PlotTitle) +
            geom_line(data=svSub, aes(y=aantal,     #lijn studies
              group=iscedCode.iscedNaam,
              color=iscedCode.iscedNaam)) + 
            geom_point(data=svSub,aes(y=aantal, 
              group=iscedCode.iscedNaam,
              color=iscedCode.iscedNaam)) +
            geom_line(data=totaalaantal, aes(y=aantal)  #totaal lijn
               , color = "black") + 
            geom_point(data=totaalaantal, aes(y=aantal) 
                , color = "black") +
            geom_line(data=totaalaantalselect, aes(y=aantal)  #totaal select lijn
               , color = "gray48") + 
            geom_point(data=totaalaantalselect, aes(y=aantal) 
              , color = "gray48") +
            labs(color = "Studierichting")+ 
            theme(legend.position="none")
        }

        else if (input$StudentenEerstejaars_Totaalselect == TRUE ){
          #alleen select
          ##select lijn
          totaalaantalselect <- TotaalAantalSelect(data =studievoortgang, 
                                                   selectInput = input$StudentenPerSector_selectStudyImp, 
                                                   filterParams= c("jaartal"))

          ggplot(svSub, aes(x=jaartal)) + 
            xlab("Jaar") +  
            ylab("Aantal studenten") + 
            ggtitle(PlotTitle) +
            geom_line(data=svSub, aes(y=aantal,     #lijn studies
                                      group=iscedCode.iscedNaam,
                                      color=iscedCode.iscedNaam)) + 
            geom_point(data=svSub,aes(y=aantal, 
                                      group=iscedCode.iscedNaam,
                                      color=iscedCode.iscedNaam)) +
            geom_line(data=totaalaantalselect, aes(y=aantal)  #totaal select lijn
                      , color = "gray48") + 
            geom_point(data=totaalaantalselect, aes(y=aantal) 
                       , color = "gray48") +
            labs(color = "Studierichting")+ 
            theme(legend.position="none")
          
          
        }
        else if (input$StudentenEerstejaars_Totaal == TRUE ){
          #Totaal berekenen
          totaalaantal <- TotaalAantal(data =studievoortgang, 
                                       filterParams= c("jaartal"))
          ggplot(svSub, aes(x=jaartal)) + 
            xlab("Jaar") +  
            ylab("Aantal studenten") + 
            ggtitle(PlotTitle) +
            geom_line(data=svSub, aes(y=aantal,     #lijn studies
                                      group=iscedCode.iscedNaam,
                                      color=iscedCode.iscedNaam)) + 
            geom_point(data=svSub,aes(y=aantal, 
                                      group=iscedCode.iscedNaam,
                                      color=iscedCode.iscedNaam)) +
            geom_line(data=totaalaantal, aes(y=aantal)  #totaal lijn
                      , color = "black") + 
            geom_point(data=totaalaantal, aes(y=aantal) 
                       , color = "black") +
            labs(color = "Studierichting")+ 
            theme(legend.position="none")
          
          
        }
        else{
          #plotten
          ggplot(svSub, aes(x=jaartal)) + 
            xlab("Jaar") +  
            ylab("Aantal studenten") + 
            ggtitle(PlotTitle) +
            geom_line(data=svSub, aes(y=aantal,     #lijn studies
                                      group=iscedCode.iscedNaam,
                                      color=iscedCode.iscedNaam)) + 
            geom_point(data=svSub,aes(y=aantal, 
                                      group=iscedCode.iscedNaam,
                                      color=iscedCode.iscedNaam)) +
            labs(color = "Studierichting")+ 
            theme(legend.position="none")
        }
        
        
        
        
  })
  
  output$StudentenPerSector_aantalStudentenBarPlot <- renderPlot({
    
    
    svBarSub <- studievoortgang[studievoortgang$iscedCode.iscedNaam %in% input$StudentenPerSector_selectStudyImp,]
    
    PlotTitle <- "Aantal eerstejaarsstudenten \nper jaar verdeeld per studie"
    
    #     
    #         ggplot(svSub, aes(x=svSub$jaartal, y=svSub$aantal), environment=environment()) +
    #           xlab("Jaar") +
    #           ylab("Aantal Studenten") +
    #           geom_bar(stat = "identity", fill="red", alpha = 1/2)+
    #           ggtitle(plotTitle)
    #       
    #         ggplot(svSub, aes(x=svSub$jaartal, y=svSub$aantal, fill=svSub$iscedCode$iscedNaam), environment=environment()) +
    #           xlab("Jaar") +
    #           ylab("Aantal Studenten") +
    #           geom_bar(stat = "identity")+
    #           ggtitle(plotTitle) +
    #           scale_fill_manual(values=rainbow(length(input$StudentenPerSector_selectStudyImp)),name="Opleidings Sector")
    #       
    
    if (input$StudentenEerstejaars_Totaal == TRUE & input$StudentenEerstejaars_Totaalselect == TRUE ){ 
      ##allebei de lijnen
      ##select lijn
      totaalaantalselect <- TotaalAantalSelect(data =studievoortgang, 
                                               selectInput = input$StudentenPerSector_selectStudyImp, 
                                               filterParams= c("jaartal"))
      
      #Totaal berekenen
      totaalaantal <- TotaalAantal(data =studievoortgang, 
                                   filterParams= c("jaartal"))
      
      ggplot(svBarSub, aes(x=jaartal)) + 
        xlab("Jaar") +  
        ylab("Aantal studenten") + 
        ggtitle(PlotTitle) +
        geom_bar(data=svBarSub, stat = "identity",
                                  aes(y=aantal,fill=iscedCode.iscedNaam)) +
        geom_line(data=totaalaantalselect, aes(y=aantal  #totaal select lijn
                  , color = "gray48")) + 
        geom_point(data=totaalaantalselect, aes(y=aantal 
                   , color = "gray48")) +
        geom_line(data=totaalaantal, aes(y=aantal  #totaal lijn
                  , color = "black")) + 
        geom_point(data=totaalaantal, aes(y=aantal 
                   , color = "black")) +
        scale_color_manual(values=c("black","gray48"),breaks=c("black","gray48"), labels=c("Totaallijn","Totaallijn geselecteerde"))+
        labs(color = "Totaallijn")+
        labs(fill = "Studierichting")
    }
    

    else if (input$StudentenEerstejaars_Totaalselect == TRUE ){
      ##select lijn
      totaalaantalselect <- TotaalAantalSelect(data =studievoortgang, 
                                               selectInput = input$StudentenPerSector_selectStudyImp, 
                                               filterParams= c("jaartal"))

      ggplot(svBarSub, aes(x=jaartal)) + 
        xlab("Jaar") +  
        ylab("Aantal studenten") + 
        ggtitle(PlotTitle) +
        geom_bar(data=svBarSub, stat = "identity",
                 aes(y=aantal,fill=iscedCode.iscedNaam)) +
        geom_line(data=totaalaantalselect, aes(y=aantal  #totaal select lijn
                  , color = "gray48")) + 
        geom_point(data=totaalaantalselect, aes(y=aantal 
                   , color = "gray48")) +
        scale_color_manual(values=c("gray48"),breaks=c("gray48"), labels=c("Totaallijn geselecteerde"))+
        labs(color = "Totaallijn")+
        labs(fill = "Studierichting")
      
      
    }
    else if (input$StudentenEerstejaars_Totaal == TRUE ){
      #Totaal berekenen
      totaalaantal <- TotaalAantal(data =studievoortgang, 
                                   filterParams= c("jaartal"))
      ggplot(svBarSub, aes(x=jaartal)) + 
        xlab("Jaar") +  
        ylab("Aantal studenten") + 
        ggtitle(PlotTitle) +
        geom_bar(data=svBarSub, stat = "identity",
                 aes(y=aantal,fill=iscedCode.iscedNaam)) +
        geom_line(data=totaalaantal, aes(y=aantal  #totaal lijn
                  , color = "black")) + 
        geom_point(data=totaalaantal, aes(y=aantal 
                   , color = "black")) +
        scale_color_manual(values=c("black"),breaks=c("black"), labels=c("Totaallijn"))+
        labs(color = "Totaallijn")+
        labs(fill = "Studierichting")
      
      
    }
    else{
      #plotten
      ggplot(svBarSub, aes(x=jaartal)) + 
        xlab("Jaar") +  
        ylab("Aantal studenten") + 
        ggtitle(PlotTitle) +
        geom_bar(data=svBarSub, stat = "identity",
                 aes(y=aantal,fill=iscedCode.iscedNaam)) +
        labs(fill = "Studierichting")
    }
  })      
 
  #########################
  ## VOORSPELLINGEN PLOT ##
  #########################
  
  output$StudentenPerSector_aantalStudentenVoorspellingPlot <- renderPlot({
    
    svSub <- studievoortgang[studievoortgang$iscedCode.iscedNaam %in% input$StudentenPerSector_selectStudyImp,]
    StudentenEerstejaars_forecastSub <- createForecastSub(svSub, "iscedCode.iscedNaam", 1995, 2012,"")
    
    #totaallijn
    totaalaantal <- TotaalAantal(data =studievoortgang, 
                                 filterParams= c("jaartal"))
    forecastTotaal         <- createForecastSub(totaalaantal, "totaal", 1995, 2012, "")
    forecastTotaal$soort   = "Totaal gediplomeerden" 
    
    
    StudentenEerstejaars_forecast_baseplot <- ggplot(StudentenEerstejaars_forecastSub, aes(x=jaartal)) +
      xlab("Jaar") + 
      ylab("Aantal eerstejaars studenten") +
      ggtitle("Aantal eerstejaars studenten per studiesector") +
      geom_line(linetype="dashed", size=1,
                aes(y=fitted,
                    group=iscedCode.iscedNaam,
                    color=iscedCode.iscedNaam))+
      geom_line(aes(y=aantal, 
                    group=iscedCode.iscedNaam,
                    color=iscedCode.iscedNaam))+
      geom_point(aes(y=aantal, 
                     group=iscedCode.iscedNaam,
                     color=iscedCode.iscedNaam))+
      labs(color = "Studiesector")
    
    if (input$StudentenEerstejaars_Totaal == TRUE & input$StudentenEerstejaars_TotaalSelect == TRUE ){ 
      ##allebei de lijnen
      #selectlijn
      totaalaantalselect <- TotaalAantalSelect(data =studievoortgang, 
                                               selectInput = input$StudentenPerSector_selectStudyImp, 
                                               filterParams= c("jaartal"))
      forecastTotaalselect         <- createForecastSub(totaalaantalselect, "totaal", 1995, 2012, "")
      forecastTotaalselect$soort   = "Totaal eerstejaars studenten"
      
      StudentenEerstejaars_forecast_baseplot +
        #TOTAAL GESELECTEERD
        geom_line(data=forecastTotaalselect, aes(y=aantal, 
                                                 group=soort,
                                                 color=soort), color = "gray48") + 
        geom_point(data=forecastTotaalselect, aes(y=aantal, 
                                                  group=soort,
                                                  color=soort), color = "gray48") +
        geom_line(data=forecastTotaalselect, linetype="dashed", size=1,
                  aes(y=fitted, group=soort, color=soort), color = "gray48") +
        
        geom_ribbon(data=forecastTotaalselect, aes(ymin=lo80, ymax=hi80, x=jaartal, group=soort), fill="blue", alpha=.25) +
        geom_ribbon(data=forecastTotaalselect, aes(ymin=lo95, ymax=hi95, x=jaartal, group=soort), fill="darkblue", alpha=.25) +
        #TOTAAL
        geom_line(data=forecastTotaal, aes(y=aantal, 
                                           group=soort,
                                           color=soort), color = "black") + 
        geom_point(data=forecastTotaal, aes(y=aantal, 
                                            group=soort,
                                            color=soort), color = "black") +
        geom_line(data=forecastTotaal, linetype="dashed", size=1,
                  aes(y=fitted, group=soort, color=soort), color = "black") + 
        
        geom_ribbon(data=forecastTotaal, aes(ymin=lo80, ymax=hi80, x=jaartal, group=soort), fill="red", alpha=.25) +
        geom_ribbon(data=forecastTotaal, aes(ymin=lo95, ymax=hi95, x=jaartal, group=soort), fill="darkred", alpha=.25)
      
    }
    else if (input$StudentenEerstejaars_TotaalSelect == TRUE ){
      #alleen select
      totaalaantalselect <- TotaalAantalSelect(data =studievoortgang, 
                                               selectInput = input$StudentenPerSector_selectStudyImp, 
                                               filterParams= c("jaartal"))
      forecastTotaalselect         <- createForecastSub(totaalaantalselect, "totaal", 1995, 2012, "")
      forecastTotaalselect$soort   = "Totaal eerstejaars studenten"
      
      StudentenEerstejaars_forecast_baseplot +
        #TOTAAL GESELECTEERD
        geom_line(data=forecastTotaalselect, aes(y=aantal, 
                                                 group=soort,
                                                 color=soort), color = "gray48") + 
        geom_point(data=forecastTotaalselect, aes(y=aantal, 
                                                  group=soort,
                                                  color=soort), color = "gray48") +
        geom_line(data=forecastTotaalselect, linetype="dashed", size=1,
                  aes(y=fitted, group=soort, color=soort), color = "gray48") +
        
        geom_ribbon(data=forecastTotaalselect, aes(ymin=lo80, ymax=hi80, x=jaartal, group=soort), fill="blue", alpha=.25) +
        geom_ribbon(data=forecastTotaalselect, aes(ymin=lo95, ymax=hi95, x=jaartal, group=soort), fill="darkblue", alpha=.25)
      
    }
    else if (input$StudentenEerstejaars_Totaal == TRUE ){
      #alleen totaal
      
      StudentenEerstejaars_forecast_baseplot +
        #TOTAAL
        geom_line(data=forecastTotaal, aes(y=aantal, 
                                           group=soort,
                                           color=soort), color = "black") + 
        geom_point(data=forecastTotaal, aes(y=aantal, 
                                            group=soort,
                                            color=soort), color = "black") +
        geom_line(data=forecastTotaal, linetype="dashed", size=1,
                  aes(y=fitted, group=soort, color=soort), color = "black") + 
        
        geom_ribbon(data=forecastTotaal, aes(ymin=lo80, ymax=hi80, x=jaartal, group=soort), fill="red", alpha=.25) +
        geom_ribbon(data=forecastTotaal, aes(ymin=lo95, ymax=hi95, x=jaartal, group=soort), fill="darkred", alpha=.25)
      
    }
    else{
      StudentenEerstejaars_forecast_baseplot
    }
    
  })
  
  
  
  observe({
    trueFalse = length(input$StudentenPerSector_selectStudyImp) == length(unique(studievoortgang$iscedCode.iscedNaam))

    updateCheckboxInput(session, "StudentenPerSector_AlleStudies", value = trueFalse)
    
  })
  observeEvent(input$StudentenPerSector_AlleStudies, {
    trueFalse = length(input$StudentenPerSector_selectStudyImp) == length(unique(studievoortgang$iscedCode.iscedNaam))
    if(input$StudentenPerSector_AlleStudies == T && !trueFalse){
      updateSelectInput(session, "StudentenPerSector_selectStudyImp",
                        selected = studievoortgang$iscedCode.iscedNaam
      )
    }
  })
}