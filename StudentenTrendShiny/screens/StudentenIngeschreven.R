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
      colnames(siSub)<-c("soort","jaartal","aantal")
      filterParams <- c('jaartal')
      rev <- FALSE
    } else {
      colnames(siSub)[colnames(siSub)=="iscedCode.iscedNaam"] <- "soort"
      filterParams <- c("ondCode",'jaartal')
      rev <- TRUE
    }
    
    #data aanpassen nav keuze gebruiker: studie(s)
    siSub <- siSub[siSub$soort %in% reac$selections,]
    plotTitle <- paste("Aantal ingeschreven bachelor studenten per jaar verdeeld per studie")
    
    #Totaal geselecteerd lijn toevoegen
    if (input$StudentenIngeschreven_Totaalselect == TRUE && length(reac$selections) != 0){
      siSub <- TotaalAantalSelect(data = siSub,
                                  studieNiveauInput = input$StudentenIngeschreven_StudieNiveau,
                                  filterParams=filterParams)
    }
    
    #Totaal lijn toevoegen
    if (input$StudentenIngeschreven_Totaal == TRUE ){
      #totaallijn
      siSub <- TotaalAantal(data = studenten_ingeschrevenen,
                            subSet = siSub,
                            studieNiveauInput = input$StudentenIngeschreven_StudieNiveau, 
                            filterParams = filterParams)
    }
    
    #Basis plot maken
    plot <- ggplot(siSub, aes(x=jaartal)) + 
      xlab("Jaar") +  
      ylab("Aantal studenten") + 
      ggtitle(plotTitle) +
      theme(legend.position="none") +
      geom_line(data=siSub, 
                aes(y=aantal, group=soort, color=soort), size=-1) + 
      geom_point(data=siSub,
                 aes(y=aantal, group=soort, color=soort), size=-1) +
      scale_color_manual(values=GetColors(siSub$soort, rev), labels=unique(siSub$soort))
    
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
      filterParams <- c('jaartal')
    } else {
      filterParams <- c("ondCode",'jaartal')
    }
    
    #data aanpassen nav keuze gebruiker: studie(s)
    siBarSub <- siBarSub[siBarSub$iscedCode.iscedNaam %in% reac$selections,]
    plotTitle <- paste("Aantal ingeschreven bachelor studenten per jaar verdeeld per studie")
    
    #Totaal selectlijn
    if (input$StudentenIngeschreven_Totaalselect == TRUE && length(reac$selections) != 0){
      siBarSub <- TotaalAantalSelect(data = siBarSub,
                                     studieNiveauInput = input$StudentenIngeschreven_StudieNiveau,
                                     filterParams= filterParams)
    } 
    
    #Totaallijn 
    if (input$StudentenIngeschreven_Totaal == TRUE ){
      siBarSub <- TotaalAantal(data = studenten_ingeschrevenen, 
                               subSet = siBarSub,
                               studieNiveauInput = input$StudentenIngeschreven_StudieNiveau, 
                               filterParams= filterParams)
    } 
    
    plot <- ggplot(siBarSub, aes(x=jaartal)) + 
      xlab("Jaar") +  
      ylab("Aantal studenten") + 
      ggtitle(plotTitle) +
      geom_bar(stat = "identity", aes(y=aantal, fill=iscedCode.iscedNaam))+
      scale_fill_manual(values=GetColors(siBarSub$iscedCode.iscedNaam), name="Studiesector")
    
    AddTotaalLines(plot=plot, data=siBarSub)
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
      colnames(siSub)<-c("soort","jaartal","aantal")
      filterParams <- c('jaartal')
    } else {
      colnames(siSub)[colnames(siSub)=="iscedCode.iscedNaam"] <- "soort"
      filterParams <- c("ondCode",'jaartal')
    }
    
    #data aanpassen nav keuze gebruiker: studie(s) en forecast maken
    siSub <- siSub[siSub$soort %in% reac$selections,]
    
    #Totaalselectlijn
    if (input$StudentenIngeschreven_Totaalselect == TRUE && length(reac$selections) != 0){
      siSub <- TotaalAantalSelect(data = siSub,
                                  studieNiveauInput = input$StudentenIngeschreven_StudieNiveau, 
                                  filterParams= filterParams)
    }
     
    #Totaallijn
    if (input$StudentenIngeschreven_Totaal == TRUE ){
      siSub <- TotaalAantal(data = studenten_ingeschrevenen,
                            subSet = siSub,
                            studieNiveauInput = input$StudentenIngeschreven_StudieNiveau, 
                            filterParams= filterParams)
    }
    
    StudentenIngeschreven_forecastSub <- createForecastSub(siSub, "aantal", "soort", 1990, 2014,"")
    
    plot <- ggplot(StudentenIngeschreven_forecastSub, aes(x=jaartal)) +
      ggtitle("Aantal ingeschreven studenten per studiesector") +
      xlab("Jaar") + 
      ylab("Aantal ingeschreven studenten")
    
    AddTotaalLines(plot=plot, data=StudentenIngeschreven_forecastSub, forecast=T, name="Studiesector")  
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
  