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
                              choices = list(Studiesectoren = unique(studievoortgang$iscedCode.iscedNaam), "Selecteer een studiesector" = ""),
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
  reac <- reactiveValues(redraw = TRUE, selections = isolate(input$StudentenPerSector_selectStudyImp))
  
  output$StudentenPerSector_aantalStudentenPlot <- renderPlotly({
    svSub <- studievoortgang[studievoortgang$iscedCode.iscedNaam %in% reac$selections,]
    PlotTitle <- "Aantal eerstejaarsstudenten per jaar verdeeld per studie"
    
    plot <- ggplot(svSub, aes(x=jaartal)) + 
      xlab("Jaar") +  
      ylab("Aantal studenten") + 
      ggtitle(PlotTitle) +
      theme(legend.position="none")
    
    #scale_color_manual options
    scmOptionsList <- InitGGLegend()
    
    if(length(reac$selections) != 0) {
      plot <- plot +
        geom_line(data=svSub, aes(y=aantal,     #lijn studies
                                  group=iscedCode.iscedNaam,
                                  color=iscedCode.iscedNaam), size = -1) + 
        geom_point(data=svSub,aes(y=aantal, 
                                  group=iscedCode.iscedNaam,
                                  color=iscedCode.iscedNaam))
      
      scmOptionsList$values <- c(scmOptionsList$values, GetColors(svSub$iscedCode.iscedNaam))
      scmOptionsList$breaks <- c(scmOptionsList$breaks, GetColors(svSub$iscedCode.iscedNaam))
      scmOptionsList$labels <- c(scmOptionsList$labels, unique(svSub$iscedCode.iscedNaam))
    }
    
    #Totaal berekenen
    if(input$StudentenEerstejaars_Totaal == TRUE) {
      totaalaantal <- TotaalAantal(data          = studievoortgang
                                   ,filterParams = c("jaartal"))
      
      TotaalLine <- AddTotaalLine(plot=plot, 
                                  data=totaalaantal, 
                                  colors=scmOptionsList, 
                                  size=-1)
      
      plot           <- TotaalLine$plot
      scmOptionsList <- TotaalLine$colors
    }
    
    #Totaal selectlijn
    if (input$StudentenEerstejaars_Totaalselect == TRUE && length(reac$selections) != 0){ 
      totaalaantalselect <- TotaalAantalSelect(data          = svSub
                                               ,filterParams = c("jaartal"))
      
      TotaalSelectLine <- AddTotaalSelectLine(plot=plot, 
                                              data=totaalaantalselect, 
                                              colors=scmOptionsList, 
                                              size=-1)
      
      plot           <- TotaalSelectLine$plot
      scmOptionsList <- TotaalSelectLine$colors
    }
    
    plot <- plot +
      scale_color_manual(values=scmOptionsList$values, labels=scmOptionsList$labels)
    
    #Render de plot
    if(length(reac$selections) != 0 || input$StudentenEerstejaars_Totaal == TRUE) {
      PrintGGPlotly(plot)
    } else {
      return(plot)
    }
    
    PrintGGPlotly(plot)
  })
  
  output$StudentenPerSector_aantalStudentenBarPlot <- renderPlot({
    svSub <- studievoortgang[studievoortgang$iscedCode.iscedNaam %in% reac$selections,]
    PlotTitle <- "Aantal eerstejaarsstudenten per jaar verdeeld per studie"
    
    plot <- ggplot(svSub, aes(x=jaartal)) + 
      xlab("Jaar") +  
      ylab("Aantal studenten") + 
      ggtitle(PlotTitle)
    
    #scale_color_manual options
    scmOptionsList <- InitGGLegend()
    
    if(length(reac$selections) != 0) {
      plot <- plot +
        geom_bar(stat = "identity", aes(y=aantal,fill=iscedCode.iscedNaam)) + 
        scale_fill_manual(values=GetColors(svSub$iscedCode.iscedNaam), name="Studiesector")
    }
    
    #Totaal berekenen
    if (input$StudentenEerstejaars_Totaal == TRUE){ 
      totaalaantal <- TotaalAantal(data = studievoortgang, filterParams= c("jaartal"))
      TotaalLine   <- AddTotaalLine(plot = plot, data = totaalaantal, colors=scmOptionsList)
      
      plot           <- TotaalLine$plot
      scmOptionsList <- TotaalLine$colors
    }
    
    #select lijn
    if(input$StudentenEerstejaars_Totaalselect == TRUE && length(reac$selections) != 0) {
      totaalaantalselect <- TotaalAantalSelect(data = svSub, filterParams= c("jaartal"))
      TotaalSelectLine   <- AddTotaalSelectLine(plot = plot, data = totaalaantalselect, colors=scmOptionsList)
      
      plot           <- TotaalSelectLine$plot
      scmOptionsList <- TotaalSelectLine$colors
    }
    
    plot +
      scale_color_manual(values=scmOptionsList$values, labels=scmOptionsList$labels)
  })      

  #########################
  ## VOORSPELLINGEN PLOT ##
  #########################
  output$StudentenPerSector_aantalStudentenVoorspellingPlot <- renderPlot({
    svSub <- studievoortgang[studievoortgang$iscedCode.iscedNaam %in% reac$selections,]
    StudentenEerstejaars_forecastSub <- createForecastSub(svSub, "aantal", "iscedCode.iscedNaam", 1995, 2012,"")
    
    plot <- ggplot(StudentenEerstejaars_forecastSub, aes(x=jaartal)) +
      xlab("Jaar") + 
      ylab("Aantal eerstejaars studenten") +
      ggtitle("Aantal eerstejaars studenten per studiesector")

    #scale_color_manual options
    scmOptionsList         <- InitGGLegend()
    sfillmanualOptionsList <- InitGGLegend()
    
    if(length(reac$selections) != 0) {
      plot <- plot +
        geom_line(linetype="dashed", size=1,
                  aes(y=fitted, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam)) +
        geom_line(aes(y=aantal, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam)) +
        geom_point(aes(y=aantal, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam)) +
        scale_color_manual(values=GetColors(svSub$iscedCode.iscedNaam), name = "Studiesector") 
      
      scmOptionsList$values <- c(scmOptionsList$values, GetColors(StudentenEerstejaars_forecastSub$iscedCode.iscedNaam))
      scmOptionsList$breaks <- c(scmOptionsList$breaks, GetColors(StudentenEerstejaars_forecastSub$iscedCode.iscedNaam))
      scmOptionsList$labels <- c(scmOptionsList$labels, unique(StudentenEerstejaars_forecastSub$iscedCode.iscedNaam))
    }
    
    if (input$StudentenEerstejaars_Totaal == TRUE ){
      #totaallijn
      totaalaantal   <- TotaalAantal(data =studievoortgang, 
                                     filterParams= c("jaartal"))
      forecastTotaal <- createForecastSub(totaalaantal, "aantal", "singleColumn", 1995, 2012, "")
      TotaalLine     <- AddTotaalLine(plot=plot, 
                                      data=forecastTotaal, 
                                      colors=scmOptionsList, 
                                      fills =sfillmanualOptionsList,
                                      forecast=TRUE, size=1)
      
      plot                   <- TotaalLine$plot
      scmOptionsList         <- TotaalLine$colors
      sfillmanualOptionsList <- TotaalLine$fills
    }
    
    if (input$StudentenEerstejaars_Totaalselect == TRUE && length(reac$selections) != 0){
      #alleen select
      totaalaantalselect <- TotaalAantalSelect(data =studievoortgang, 
                                               selectInput = reac$selections, 
                                               filterParams= c("jaartal"))
      
      forecastTotaalselect  <- createForecastSub(totaalaantalselect, "aantal", "singleColumn", 1995, 2012, "")
      TotaalSelectLine      <- AddTotaalSelectLine(plot=plot, 
                                                   data=forecastTotaalselect, 
                                                   colors=scmOptionsList, 
                                                   fills =sfillmanualOptionsList,
                                                   forecast=TRUE, size=1)
      
      plot                   <- TotaalSelectLine$plot
      scmOptionsList         <- TotaalSelectLine$colors
      sfillmanualOptionsList <- TotaalSelectLine$fills
    }
    
    #Render de plot
    plot +
      scale_color_manual(values=scmOptionsList$values, labels=scmOptionsList$labels, name="Studiesector") +
      scale_fill_manual(values=sfillmanualOptionsList$values, labels=sfillmanualOptionsList$labels, name="Betrouwbaarheidsinterval")
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
  
  observe({
    input$StudentenPerSector_selectStudyImp
    
    reac$redraw <- FALSE
  })
  
  observe({
    invalidateLater(500, session)
    input$StudentenPerSector_selectStudyImp
    input$redraw
    if (isolate(reac$redraw)) {
      reac$selections <- input$StudentenPerSector_selectStudyImp
    } else {
      isolate(reac$redraw <- TRUE)
    }
  })
  
}