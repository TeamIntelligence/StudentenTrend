StudentenIngeschrevenUI <- function(PageName){
  return(  
    tabItem(tabName = PageName,
      # Page title
      fluidRow(
        box(width = 12, collapsible = T, title="Ingeschreven studenten",
            p("Op deze pagina vindt u het aantal ingeschreven studenten per studiesector over de periode 1990 tot en met 2014. U kunt zelf kiezen welke studiesectoren u wilt weergeven. Daarnaast kunt u ook zelf een keuze maken tussen HBO en WO. Verder kan u ook een totaallijn weergeven van alle studies of een totaallijn van de studies die u geselecteerd hebt."),
            p("De grafiek biedt inzicht hoeveel studenten elk jaar ingeschreven zijn voor een studie binnen een bepaalde studiesector. Er kan vervolgens uit opgemaakt worden hoe een studie binnen een bepaalde studiesector zich ontwikkeld qua grootte.")
            ),
        box(width=4, height = 170, 
          selectInput("StudentenIngeschreven_SelectStudyImp",
                      "Selecteer een of meerdere studiesectoren om weer te geven:",
                      choices = studenten_ingeschrevenen$iscedCode.iscedNaam,
                      multiple = TRUE,
                      selectize = TRUE
          ),
          checkboxInput("StudentenIngeschreven_AlleStudies",
                        "Selecteer alle studies"
          )
        ),
        box(width = 4, height = 170,
          radioButtons("StudentenIngeschreven_StudieNiveau",
                       "Studie Niveau", 
                       choices = list("HBO" = "HBO", 
                                      "WO"  = "WO",
                                      "HBO en WO" = "HBOWO")
          )
        ),
        box(width = 4, height = 170,
          checkboxInput("StudentenIngeschreven_Totaalselect",
                        "Totaal lijn weergeven van de geselecteerde studies"
          ),
          checkboxInput("StudentenIngeschreven_Totaal",
                      "Totaal lijn weergeven"
          )
          
        ),
        
        tabBox(width=12, height=550, 
               tabPanel("Huidige data",
                        box(width=5, plotlyOutput("aantalIngeschrevenPlot", height=450)),
                        box(width=7, plotOutput("aantalIngeschrevenBarPlot", height=450))
               ),
               tabPanel("Voorspelling",
                        box(width=12,plotOutput("aantalIngeschrevenVoorspellingPlot", height = 450))
               )
        )
      )
    )
  )
}

StudentenIngeschrevenServer <- function(input, output, session){
  
  reac <- reactiveValues(redraw = TRUE, selections = isolate(input$StudentenIngeschreven_SelectStudyImp))
  
  #######################
  ## NORMALE LINE PLOT ##
  #######################
  output$aantalIngeschrevenPlot <- renderPlotly({
    #data aanpassen nav keuzes gebruiker: studieniveau
    siSub <- switch (input$StudentenIngeschreven_StudieNiveau,
                     "HBO" = studenten_ingeschrevenen[studenten_ingeschrevenen$ondCode == "HBO",],
                     "WO" = studenten_ingeschrevenen[studenten_ingeschrevenen$ondCode == "WO",],
                     "HBOWO" = aggregate(studenten_ingeschrevenen$aantal, by=list(iscedNaam=studenten_ingeschrevenen$iscedCode.iscedNaam,jaartal=studenten_ingeschrevenen$jaartal), FUN=sum)
    )
    
    #namen kolomtitels van de nieuwe gevormde data aanpassen
    if(input$StudentenIngeschreven_StudieNiveau == "HBOWO"){
      colnames(siSub)<-c("iscedCode.iscedNaam","jaartal","aantal")
    }
    
    #data aanpassen nav keuze gebruiker: studie(s)
    siSub <- siSub[siSub$iscedCode.iscedNaam %in% reac$selections,]
    
    #plotten en titel laten afhangen
    plotTitle <- paste("Aantal ingeschreven bachelor studenten \nper jaar verdeeld per studie")
    
    #scale_color_manual options
    scmOptionsList <- InitGGLegend()
    
    #Basis plot maken
    plot <- ggplot(siSub, aes(x=jaartal)) + 
      xlab("Jaar") +  
      ylab("Aantal studenten") + 
      ggtitle(plotTitle) +
      theme(legend.position="none")
    
    if(length(reac$selections) != 0) {
      plot <- plot +
        geom_line(data=siSub, 
                  aes(y=aantal, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam), size=-1) + 
        geom_point(data=siSub,
                   aes(y=aantal, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam), size=-1) +
        scale_color_manual(values=GetColors(siSub$iscedCode.iscedNaam))
      
      scmOptionsList$values <- c(scmOptionsList$values, GetColors(siSub$iscedCode.iscedNaam))
      scmOptionsList$breaks <- c(scmOptionsList$breaks, GetColors(siSub$iscedCode.iscedNaam))
      scmOptionsList$labels <- c(scmOptionsList$labels, unique(siSub$iscedCode.iscedNaam))
    }
    
    #Totaal lijn toevoegen
    if (input$StudentenIngeschreven_Totaal == TRUE ){
      #totaallijn
      totaalaantal <- TotaalAantal(data = studenten_ingeschrevenen,
                                   studieNiveauInput = input$StudentenIngeschreven_StudieNiveau, 
                                   filterParams= c("ondCode",'jaartal'))
      
      TotaalLine <- AddTotaalLine(plot=plot, 
                                  data=totaalaantal, 
                                  colors=scmOptionsList, 
                                  size=-1,
                                  color="black")
      
      plot           <- TotaalLine$plot
      scmOptionsList <- TotaalLine$colors
    }
    
    #Totaal geselecteerd lijn toevoegen
    if (input$StudentenIngeschreven_Totaalselect == TRUE && length(reac$selections) != 0){
      
      totaalaantalselect <- TotaalAantalSelect(data = studenten_ingeschrevenen,
                                               selectInput = reac$selections, 
                                               studieNiveauInput = input$StudentenIngeschreven_StudieNiveau, 
                                               filterParams= c("ondCode",'jaartal'))
      
      TotaalSelectLine <- AddTotaalSelectLine(plot=plot, 
                                              data=totaalaantalselect, 
                                              colors=scmOptionsList, 
                                              size=-1,
                                              color="gray")
      
      plot           <- TotaalSelectLine$plot
      scmOptionsList <- TotaalSelectLine$colors
    }
    
    plot <- plot +
      scale_color_manual(values=scmOptionsList$values, labels=scmOptionsList$labels)
    
    #Render de plot
    if( length(reac$selections) != 0 || input$StudentenIngeschreven_Totaal == TRUE) {
      PrintGGPlotly(plot)
    } else {
      return(plot)
    }
  })
  
  ######################
  ## NORMALE BAR PLOT ##
  ######################
  output$aantalIngeschrevenBarPlot <- renderPlot({
    #data aanpassen nav keuzes gebruiker: studieniveau
    siBarSub <- switch (input$StudentenIngeschreven_StudieNiveau,
                        "HBO" = studenten_ingeschrevenen[studenten_ingeschrevenen$ondCode == "HBO",],
                        "WO" = studenten_ingeschrevenen[studenten_ingeschrevenen$ondCode == "WO",],
                        "HBOWO" = aggregate(studenten_ingeschrevenen$aantal, by=list(iscedNaam=studenten_ingeschrevenen$iscedCode.iscedNaam,jaartal=studenten_ingeschrevenen$jaartal), FUN=sum)
    )
    
    #namen kolomtitels van de nieuwe gevormde data aanpassen
    if(input$StudentenIngeschreven_StudieNiveau == "HBOWO"){
      colnames(siBarSub)<-c("iscedCode.iscedNaam","jaartal","aantal")
    } 
    
    #data aanpassen nav keuze gebruiker: studie(s)
    siBarSub <- siBarSub[siBarSub$iscedCode.iscedNaam %in% reac$selections,]
    
    #plotten en titel laten afhangen
    plotTitle <- paste("Aantal ingeschreven bachelor studenten \nper jaar verdeeld per studie")
    
    #scale_color_manual options
    scmOptionsList <- InitGGLegend()
    
    #scale_color_manual options
    scmOptionsList.names <- c("values", "breaks", "labels")
    scmOptionsList <- setNames(vector("list", length(scmOptionsList.names )), scmOptionsList.names )
    
    plot <- ggplot(siBarSub, aes(x=jaartal)) + 
      xlab("Jaar") +  
      ylab("Aantal studenten") + 
      ggtitle(plotTitle)
    
    if (length(reac$selections) != 0) {
      plot <- plot +
        geom_bar(data = siBarSub, stat = "identity",
                 aes(y=aantal, fill=iscedCode.iscedNaam))+
        scale_fill_manual(values=GetColors(siBarSub$iscedCode.iscedNaam),name="Studiesector")
    }
      
    if (input$StudentenIngeschreven_Totaal == TRUE ){
      #totaallijn
      totaalaantal <- TotaalAantal(data = studenten_ingeschrevenen,
                                   studieNiveauInput = input$StudentenIngeschreven_StudieNiveau, 
                                   filterParams= c("ondCode",'jaartal'))
      
      TotaalLine   <- AddTotaalLine(plot = plot, data = totaalaantal, colors=scmOptionsList)
      
      plot           <- TotaalLine$plot
      scmOptionsList <- TotaalLine$colors
    } 
    
    if (input$StudentenIngeschreven_Totaalselect == TRUE && length(reac$selections) != 0){
      totaalaantalselect <- TotaalAantalSelect(data = studenten_ingeschrevenen,
                                               selectInput = reac$selections, 
                                               studieNiveauInput = input$StudentenIngeschreven_StudieNiveau, 
                                               filterParams= c("ondCode",'jaartal'))
      
      TotaalSelectLine <- AddTotaalSelectLine(plot = plot, data = totaalaantalselect, colors=scmOptionsList)
      
      plot           <- TotaalSelectLine$plot
      scmOptionsList <- TotaalSelectLine$colors
    } 
    
    #Renderplot
    plot +
      scale_color_manual(values=scmOptionsList$values, labels=scmOptionsList$labels)
  })

  #########################
  ## VOORSPELLINGEN PLOT ##
  #########################
  output$aantalIngeschrevenVoorspellingPlot <- renderPlot({
    
    #data aanpassen nav keuzes gebruiker: studieniveau
    siSub <- switch (input$StudentenIngeschreven_StudieNiveau,
                     "HBO" = studenten_ingeschrevenen[studenten_ingeschrevenen$ondCode == "HBO",],
                     "WO" = studenten_ingeschrevenen[studenten_ingeschrevenen$ondCode == "WO",],
                     "HBOWO" = aggregate(studenten_ingeschrevenen$aantal, by=list(iscedNaam=studenten_ingeschrevenen$iscedCode.iscedNaam,jaartal=studenten_ingeschrevenen$jaartal), FUN=sum)
    )
    
    #namen kolomtitels van de nieuwe gevormde data aanpassen
    if(input$StudentenIngeschreven_StudieNiveau == "HBOWO"){
      colnames(siSub)<-c("iscedCode.iscedNaam","jaartal","aantal")
    }
    
    #data aanpassen nav keuze gebruiker: studie(s) en forecast maken
    siSub<-siSub[siSub$iscedCode.iscedNaam %in% reac$selections,]
    StudentenIngeschreven_forecastSub <- createForecastSub(siSub, "aantal", "iscedCode.iscedNaam", 1990, 2014,"")
    
    #scale_color_manual options
    scmOptionsList         <- InitGGLegend()
    sfillmanualOptionsList <- InitGGLegend()
    
    plot <- ggplot(StudentenIngeschreven_forecastSub, aes(x=jaartal)) +
      xlab("Jaar") + 
      ylab("Aantal ingeschreven studenten") +
      ggtitle("Aantal ingeschreven studenten per studiesector") 

    if (length(reac$selections) != 0) {
       
      plot <- plot +
        geom_line(linetype="dashed", size=1,
                  aes(y=fitted, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam))+
        geom_line(aes(y=aantal, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam))+
        geom_point(aes(y=aantal, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam))
       
      scmOptionsList$values <- c(scmOptionsList$values, GetColors(StudentenIngeschreven_forecastSub$iscedCode.iscedNaam))
      scmOptionsList$breaks <- c(scmOptionsList$breaks, GetColors(StudentenIngeschreven_forecastSub$iscedCode.iscedNaam))
      scmOptionsList$labels <- c(scmOptionsList$labels, unique(StudentenIngeschreven_forecastSub$iscedCode.iscedNaam))
    }
     
    #alleen totaal
    if (input$StudentenIngeschreven_Totaal == TRUE ){
      #totaallijn
      totaalaantal <- TotaalAantal(data = studenten_ingeschrevenen,
                                   studieNiveauInput = input$StudentenIngeschreven_StudieNiveau, 
                                   filterParams= c("ondCode",'jaartal'))
      forecastTotaal <- createForecastSub(totaalaantal, "aantal", "singleColumn", 1990, 2014, "")
      
      TotaalLine     <- AddTotaalLine(plot=plot, 
                                      data=forecastTotaal, 
                                      colors=scmOptionsList, 
                                      fills=sfillmanualOptionsList,
                                      forecast=TRUE, size=1)
      
      plot                   <- TotaalLine$plot
      scmOptionsList         <- TotaalLine$colors
      sfillmanualOptionsList <- TotaalLine$fills
    }
    
    if (input$StudentenIngeschreven_Totaalselect == TRUE && length(reac$selections) != 0){
      totaalaantalselect <- TotaalAantalSelect(data = studenten_ingeschrevenen,
                                               selectInput = reac$selections, 
                                               studieNiveauInput = input$StudentenIngeschreven_StudieNiveau, 
                                               filterParams= c("ondCode",'jaartal'))
      
      forecastTotaalselect         <- createForecastSub(totaalaantalselect, "aantal", "singleColumn", 1990, 2014, "")
      
      TotaalSelectLine      <- AddTotaalSelectLine(plot=plot, 
                                                   data=forecastTotaalselect, 
                                                   colors=scmOptionsList, 
                                                   fills=sfillmanualOptionsList,
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
    trueFalse = length(input$StudentenIngeschreven_SelectStudyImp) == length(unique(studenten_ingeschrevenen$iscedCode.iscedNaam))
    
    updateCheckboxInput(session, "StudentenIngeschreven_AlleStudies", value = trueFalse)
    
  })
  observeEvent(input$StudentenIngeschreven_AlleStudies, {
    trueFalse = length(input$StudentenIngeschreven_SelectStudyImp) == length(unique(studenten_ingeschrevenen$iscedCode.iscedNaam))
    if(input$StudentenIngeschreven_AlleStudies == T && !trueFalse){
      updateSelectInput(session, "StudentenIngeschreven_SelectStudyImp",
                        selected = studenten_ingeschrevenen$iscedCode.iscedNaam
      )
    }
  })
  
  observe({
    input$StudentenIngeschreven_SelectStudyImp
    
    reac$redraw <- FALSE
  })
  
  observe({
    invalidateLater(500, session)
    input$StudentenIngeschreven_SelectStudyImp
    input$redraw
    if (isolate(reac$redraw)) {
      reac$selections <- input$StudentenIngeschreven_SelectStudyImp
    } else {
      isolate(reac$redraw <- TRUE)
    }
  })
  
}   
  