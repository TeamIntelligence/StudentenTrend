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
                               box(width=5, plotlyOutput("StudentenPerSector_aantalStudentenPlot", height=450)), 
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
  
  output$StudentenPerSector_aantalStudentenPlot <- renderPlotly({
    svSub <- studievoortgang[studievoortgang$iscedCode.iscedNaam %in% input$StudentenPerSector_selectStudyImp,]
    PlotTitle <- "Aantal eerstejaarsstudenten per jaar verdeeld per studie"
    
    #plotten
    plot <- ggplot(svSub, aes(x=jaartal)) + 
      xlab("Jaar") +  
      ylab("Aantal studenten") + 
      ggtitle(PlotTitle) +
      geom_line(data=svSub, size = -1, aes(y=aantal,     #lijn studies
                                           group=iscedCode.iscedNaam,
                                           color=iscedCode.iscedNaam)) + 
      geom_point(data=svSub,aes(y=aantal, 
                                group=iscedCode.iscedNaam,
                                color=iscedCode.iscedNaam)) +
      
      scale_color_manual(values=GetColors(svSub$iscedCode.iscedNaam)) + 
      theme(legend.position="none")
    
    
    #Totaal selectlijn
    if (input$StudentenEerstejaars_Totaalselect == TRUE){ 
      totaalaantalselect <- TotaalAantalSelect(data         = studievoortgang
                                               ,selectInput = StudentenPerSector_selectStudyImp
                                               ,filterParams= c("jaartal"))
      
      plot <- plot + 
        geom_line(data=totaalaantalselect, size = -1, 
                  aes(y=aantal, group=soort, color=soort), color = "gray48") + 
        geom_point(data=totaalaantalselect, 
                   aes(y=aantal, group=soort, color=soort), color = "gray48")
    }
    
    #Totaal berekenen
    if(input$StudentenEerstejaars_Totaal == TRUE) {
      totaalaantal <- TotaalAantal(data          = svSub
                                   ,filterParams = c("jaartal"))
      
      plot <- plot + 
        geom_line(data=totaalaantal, size = -1, 
                  aes(y=aantal, group=soort, color=soort),
                  color = "black") + 
        geom_point(data=totaalaantal, 
                   aes(y=aantal, group=soort, color=soort),
                   color = "black")
    }
    
    PrintGGPlotly(plot)
  })
  
  output$StudentenPerSector_aantalStudentenBarPlot <- renderPlot({
    svBarSub <- studievoortgang[studievoortgang$iscedCode.iscedNaam %in% input$StudentenPerSector_selectStudyImp,]
    PlotTitle <- "Aantal eerstejaarsstudenten per jaar verdeeld per studie"
    
    plot <- ggplot(svBarSub, aes(x=jaartal)) + 
      xlab("Jaar") +  
      ylab("Aantal studenten") + 
      ggtitle(PlotTitle) +
      geom_bar(data=svBarSub, stat = "identity",
               aes(y=aantal,fill=iscedCode.iscedNaam)) +
      scale_fill_manual(values=GetColors(svBarSub$iscedCode.iscedNaam), name="Studierichting") +
      theme(plot.title = do.call(element_text, GetDefaultTitleFont()))
    
    scale_color_manual_params <- list(values = c(), breaks = c(), labels = c())     
    
    #Totaal berekenen
    if (input$StudentenEerstejaars_Totaal == TRUE){ 
      
      totaalaantal <- TotaalAantal(data =studievoortgang, 
                                   filterParams= c("jaartal"))
      
      plot <- plot + 
        geom_line(data=totaalaantal, 
                  aes(y=aantal, color = "black")) + 
        geom_point(data=totaalaantal, 
                   aes(y=aantal, color = "black")) +
        labs(color = "Totaallijn")
      
      scale_color_manual_params[["values"]] <- c(scale_color_manual_params[["values"]], "black")
      scale_color_manual_params[["breaks"]] <- c(scale_color_manual_params[["breaks"]], "black")
      scale_color_manual_params[["labels"]] <- c(scale_color_manual_params[["labels"]], "Totaallijn")
    }
    
    #select lijn
    if(input$StudentenEerstejaars_Totaalselect == TRUE) {
      totaalaantalselect <- TotaalAantalSelect(data =studievoortgang 
                                               ,selectInput = input$StudentenPerSector_selectStudyImp 
                                               ,filterParams= c("jaartal"))
      
      plot <- plot + geom_line(data=totaalaantalselect, aes(y=aantal  #totaal select lijn
                                                            ,color = "gray48")) + 
        geom_point(data=totaalaantalselect, aes(y=aantal 
                                                ,color = "gray48")) +
        labs(color = "Totaallijn")
      
      scale_color_manual_params[["values"]] <- c(scale_color_manual_params[["values"]], "gray48")
      scale_color_manual_params[["breaks"]] <- c(scale_color_manual_params[["breaks"]], "gray48")
      scale_color_manual_params[["labels"]] <- c(scale_color_manual_params[["labels"]], "Totaallijn geselecteerde")
    }
    
    if(input$StudentenEerstejaars_Totaal == TRUE || input$StudentenEerstejaars_Totaalselect == TRUE) {
      plot <- plot + do.call(scale_color_manual, scale_color_manual_params)
    }
    
    plot
  })      

  #########################
  ## VOORSPELLINGEN PLOT ##
  #########################
  output$StudentenPerSector_aantalStudentenVoorspellingPlot <- renderPlot({
    
    svSub <- studievoortgang[studievoortgang$iscedCode.iscedNaam %in% input$StudentenPerSector_selectStudyImp,]
    StudentenEerstejaars_forecastSub <- createForecastSub(svSub, "aantal", "iscedCode.iscedNaam", 1995, 2012,"")
    
    #totaallijn
    totaalaantal <- TotaalAantal(data =studievoortgang, 
                                 filterParams= c("jaartal"))
    forecastTotaal         <- createForecastSub(totaalaantal, "aantal", "singleColumn", 1995, 2012, "")
    forecastTotaal$soort   = "Totaal gediplomeerden" 

    SEForecastBaseplot <- ggplot(StudentenEerstejaars_forecastSub, aes(x=jaartal)) +
      xlab("Jaar") + 
      ylab("Aantal eerstejaars studenten") +
      ggtitle("Aantal eerstejaars studenten per studiesector") +
      geom_line(linetype="dashed", size=1,
                aes(y=fitted, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam)) +
      geom_line(aes(y=aantal, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam)) +
      geom_point(aes(y=aantal, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam)) +
      scale_color_manual(values=GetColors(svSub$iscedCode.iscedNaam), name = "Studiesector")
    
    if (input$StudentenEerstejaars_Totaal == TRUE ){
      
      SEForecastBaseplot <- SEForecastBaseplot +
        #TOTAAL
        geom_line(data=forecastTotaal, 
                  aes(y=aantal, group=soort, color=soort),
                  color = "black") + 
        geom_point(data=forecastTotaal, 
                   aes(y=aantal, group=soort, color=soort), 
                   color = "black") +
        geom_line(data=forecastTotaal, linetype="dashed", size=1,
                  aes(y=fitted, group=soort, color=soort), 
                  color = "black") + 
        geom_ribbon(data=forecastTotaal, aes(ymin=lo80, ymax=hi80, x=jaartal, group=soort), fill="red", alpha=.25) +
        geom_ribbon(data=forecastTotaal, aes(ymin=lo95, ymax=hi95, x=jaartal, group=soort), fill="darkred", alpha=.25)
    }
    if (input$StudentenEerstejaars_Totaalselect == TRUE ){
      #alleen select
      totaalaantalselect <- TotaalAantalSelect(data =studievoortgang, 
                                               selectInput = input$StudentenPerSector_selectStudyImp, 
                                               filterParams= c("jaartal"))
      
      forecastTotaalselect         <- createForecastSub(totaalaantalselect, "aantal", "singleColumn", 1995, 2012, "")
      forecastTotaalselect$soort   = "Totaal eerstejaars studenten"
      
      SEForecastBaseplot <- SEForecastBaseplot +
        #TOTAAL GESELECTEERD
        geom_line(data=forecastTotaalselect, 
                  aes(y=aantal, group=soort, color=soort),
                  color = "gray48") + 
        geom_point(data=forecastTotaalselect, 
                   aes(y=aantal, group=soort, color=soort), 
                   color = "gray48") +
        geom_line(data=forecastTotaalselect, linetype="dashed", size=1,
                  aes(y=fitted, group=soort, color=soort), 
                  color = "gray48") +
        geom_ribbon(data=forecastTotaalselect, aes(ymin=lo80, ymax=hi80, x=jaartal, group=soort), fill="blue", alpha=.25) +
        geom_ribbon(data=forecastTotaalselect, aes(ymin=lo95, ymax=hi95, x=jaartal, group=soort), fill="darkblue", alpha=.25)
    }
    
    #Render plot
    SEForecastBaseplot
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