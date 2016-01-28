AantalVacaturesUI <- function(PageName){
  return(
    
    tabItem(tabName = PageName,
      # Page title
      fluidRow(
        box(width=12, collapsible = T, title = "Aantal vacatures", 
            p("Op deze pagina vindt u het aantal vacatures per bedrijfssector over de periode 1997 tot en met 2015. U kunt zelf kiezen welke bedrijfssectoren u wilt weergeven. Verder kan u ook een totaallijn weergeven van alle bedrijfssectoren of een totaallijn van de bedrijfssectoren die u geselecteerd hebt."),
            p("De grafiek biedt inzicht hoeveel vacatures er elk jaar zijn voor een bepaalde bedrijfssector. Er kan vervolgens uit opgemaakt worden of het aantal vacatures een groei of een daling doormaakt. Zo is de Dot-com bubble, die barstte in het jaar 2000, duidelijk te zien in de grafiek, evenals de economische crisis die in Nederland uitbrak in het jaar 2008.")
            ),
        box(width=5, height = 150, 
            
            selectInput("AantalVacatures_SelectImp",
                        "Selecteer een of meerdere bedrijfssectoren om weer te geven:",
                        choices = list(Bedrijfssectoren = unique(vacatures$sbiCode.sbiNaam), "Selecteer een bedrijfssector" = ""),
                        multiple = TRUE,
                        selectize = TRUE
            ),
            
            checkboxInput("AantalVacatures_AlleSectoren",
                          "Selecteer alle bedrijfssectoren"
            )
            
        ),
        box(width=7, height=150,
            checkboxInput("AantalVacatures_TotaalSelect",
                          "Totaal lijn weergeven van de geselecteerde bedrijfssectoren"
            ),
            
            checkboxInput("AantalVacatures_Totaal",
                          "Totaal lijn weergeven"
            )
            
        ),
        tabBox(width=12, height=550, 
          tabPanel("Huidige data",
            box(width=5,plotlyOutput("VacaPlot", height = 450)),
            box(width=7,plotOutput("VacaBarPlot", height=450))
          ),
          tabPanel("Voorspelling",
            box(width=12,plotOutput("VacaVoorspellingPlot", height = 450))
          )
        )
      )   
    )
  )
}

AantalVacaturesServer <- function(input,output, session){
  
  reac <- reactiveValues(redraw = TRUE, selections = isolate(input$AantalVacatures_SelectImp))
  
  
  #######################
  ## NORMALE LINE PLOT ##
  #######################
  output$VacaPlot <- renderPlotly({
    #Data aanpassen nav keuze bedrijfssector
    AantalVacatures_vacSub <- vacatures_jaartallen[vacatures_jaartallen$sbiCode.sbiNaam %in% reac$selections,]
    
    plot <- ggplot(AantalVacatures_vacSub, aes(x=jaartal)) +
      xlab("Jaar") + 
      ylab("Aantal vacatures") +
      ggtitle("Aantal vacatures per sector") +
      theme(legend.position="none")
    
    #scale_color_manual options
    scmOptionsList <- InitGGLegend()
    
    #Baseplot
    if(length(reac$selections) != 0) {
      plot <- plot +
        geom_line(data=AantalVacatures_vacSub,
                  aes(y=aantal, group=sbiCode.sbiNaam, color=sbiCode.sbiNaam), size=-1) +
        geom_point(data=AantalVacatures_vacSub,
                   aes(y=aantal, group=sbiCode.sbiNaam, color=sbiCode.sbiNaam))
      
      scmOptionsList$values <- c(scmOptionsList$values, GetColors(AantalVacatures_vacSub$sbiCode.sbiNaam))
      scmOptionsList$breaks <- c(scmOptionsList$breaks, GetColors(AantalVacatures_vacSub$sbiCode.sbiNaam))
      scmOptionsList$labels <- c(scmOptionsList$labels, unique(AantalVacatures_vacSub$sbiCode.sbiNaam))
    }
    
    if (input$AantalVacatures_Totaal == TRUE) {
      #Totaallijn
      totaalaantal <- TotaalAantal(data = vacatures_jaartallen,
                                   filterParams= c("jaartal") )
      
      TotaalLine <- AddTotaalLine(plot=plot, 
                                  data=totaalaantal, 
                                  colors=scmOptionsList, 
                                  size=-1,
                                  color="black")
      
      plot           <- TotaalLine$plot
      scmOptionsList <- TotaalLine$colors
    }
    
    #Als totaalselect lijn aan staat en er meer als 0 studies geselecteerd zijn
    if (input$AantalVacatures_TotaalSelect == TRUE && length(reac$selections) != 0) { 
      totaalaantalselect <- TotaalAantalSelect(data = AantalVacatures_vacSub,
                                               filterParams= c("jaartal"))
      
      TotaalSelectLine <- AddTotaalSelectLine(plot=plot, 
                                              data=totaalaantalselect, 
                                              colors=scmOptionsList, 
                                              size=-1,
                                              color="gray48")
      
      plot           <- TotaalSelectLine$plot
      scmOptionsList <- TotaalSelectLine$colors
    }

    
    plot <- plot +
      scale_color_manual(values=scmOptionsList$values, labels=scmOptionsList$labels)
    
    #Render de plot
    if( length(reac$selections) != 0 || input$AantalVacatures_Totaal == TRUE) {
      PrintGGPlotly(plot)
    } else {
      return(plot)
    }
  })
  
  ######################
  ## NORMALE BAR PLOT ##
  ######################
  output$VacaBarPlot <- renderPlot({
    #Data aanpassen nav keuze bedrijfssector
    AantalVacatures_vacBarSub <- vacatures_jaartallen[vacatures_jaartallen$sbiCode.sbiNaam %in% reac$selections,]
    
    #scale_color_manual options
    scmOptionsList <- InitGGLegend()
    
    plot <- ggplot(AantalVacatures_vacBarSub, aes(x=jaartal)) +
      xlab("Jaar") + 
      ylab("Aantal vacatures") +
      ggtitle("Aantal vacatures per sector")
      
    if(length(reac$selections) != 0) {
      plot <- plot +
        geom_bar(stat = "identity", aes(y=aantal, fill=sbiCode.sbiNaam)) + 
        scale_fill_manual(values=GetColors(AantalVacatures_vacBarSub$sbiCode.sbiNaam), name = "Bedrijfssector")
    }
    
    if (input$AantalVacatures_Totaal == TRUE ){
      #Totaallijn
      totaalaantal <- TotaalAantal(data = vacatures_jaartallen,
                                   filterParams= c("jaartal") )
      
      TotaalLine <- AddTotaalLine(plot=plot, 
                                  data=totaalaantal, 
                                  colors=scmOptionsList)
      
      plot           <- TotaalLine$plot
      scmOptionsList <- TotaalLine$colors
    } 
    
    if (input$AantalVacatures_TotaalSelect == TRUE && length(reac$selections) != 0){ 
      totaalaantalselect <- TotaalAantalSelect(data = AantalVacatures_vacBarSub,
                                               filterParams= c("jaartal"))
      
      TotaalSelectLine <- AddTotaalSelectLine(plot=plot, 
                                              data=totaalaantalselect, 
                                              colors=scmOptionsList)
      
      plot           <- TotaalSelectLine$plot
      scmOptionsList <- TotaalSelectLine$colors
    }
    
    #Render de plot
    plot +
      scale_color_manual(values=scmOptionsList$values, labels=scmOptionsList$labels)
  })
  
  #########################
  ## VOORSPELLINGEN PLOT ##
  #########################
  output$VacaVoorspellingPlot <- renderPlot({
    
    #Data aanpassen nav keuze bedrijfssector
    AantalVacatures_vacSub      <- vacatures_jaartallen[vacatures_jaartallen$sbiCode.sbiNaam %in% reac$selections,]
    aantalVacatures_forecastSub <- createForecastSub(AantalVacatures_vacSub, "aantal", "sbiCode.sbiNaam", 1997, 2014, 2015)

    #Totaal berekenen
    scmOptionsList         <- InitGGLegend()
    sfillmanualOptionsList <- InitGGLegend()
    
    plot <- ggplot(aantalVacatures_forecastSub, aes(x=jaartal)) +
      xlab("Jaar") + 
      ylab("Aantal vacatures") +
      ggtitle("Aantal vacatures per sector")
    
    if(length(reac$selections) != 0) {
      plot <- plot +
        geom_line(linetype="dashed", size=1,
                  aes(y=fitted,
                      group=sbiCode.sbiNaam,
                      color=sbiCode.sbiNaam))+
        geom_line(aes(y=aantal, 
                      group=sbiCode.sbiNaam,
                      color=sbiCode.sbiNaam))+
        geom_point(aes(y=aantal, 
                       group=sbiCode.sbiNaam,
                       color=sbiCode.sbiNaam))
      
      scmOptionsList$values <- c(scmOptionsList$values, GetColors(AantalVacatures_vacSub$sbiCode.sbiNaam))
      scmOptionsList$breaks <- c(scmOptionsList$breaks, GetColors(AantalVacatures_vacSub$sbiCode.sbiNaam))
      scmOptionsList$labels <- c(scmOptionsList$labels, unique(AantalVacatures_vacSub$sbiCode.sbiNaam))
    }

    if (input$AantalVacatures_Totaal == TRUE ){
      #totaallijn
      totaalaantal <- TotaalAantal(data = vacatures_jaartallen, 
                                   filterParams= c('jaartal'))
      forecastTotaal <- createForecastSub(totaalaantal, "aantal", "singleColumn", 1997, 2014, 2015)
      TotaalLine     <- AddTotaalLine(plot=plot, 
                                      data=forecastTotaal, 
                                      colors=scmOptionsList, 
                                      fills=sfillmanualOptionsList,
                                      forecast=TRUE, size=1)
      
      plot                   <- TotaalLine$plot
      scmOptionsList         <- TotaalLine$colors
      sfillmanualOptionsList <- TotaalLine$fills
    } 
    
    if (input$AantalVacatures_TotaalSelect == TRUE && length(reac$selections) != 0) {
      totaalaantalselect <- TotaalAantalSelect(data = aantalVacatures_forecastSub, 
                                               filterParams= c('jaartal'))
      forecastTotaalselect   <- createForecastSub(totaalaantalselect, "aantal", "singleColumn", 1997, 2014, 2015)
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
      scale_color_manual(values=scmOptionsList$values, labels=scmOptionsList$labels, name="Bedrijfssector") +
      scale_fill_manual(values=sfillmanualOptionsList$values, labels=sfillmanualOptionsList$labels, name="Betrouwbaarheidsinterval")
  })
  
  observe({
    trueFalse = length(input$AantalVacatures_SelectImp) == length(unique(vacatures$sbiCode.sbiNaam))
    updateCheckboxInput(session, "AantalVacatures_AlleSectoren", value = trueFalse)
  })
  
  observeEvent(input$AantalVacatures_AlleSectoren, {
    trueFalse = length(input$AantalVacatures_SelectImp) == length(unique(vacatures$sbiCode.sbiNaam))
    if(input$AantalVacatures_AlleSectoren == T && !trueFalse){
      updateSelectInput(session, "AantalVacatures_SelectImp",
                        selected = vacatures$sbiCode.sbiNaam
      )
    }
  })
  
  observe({
    input$AantalVacatures_SelectImp
    
    reac$redraw <- FALSE
  })
  
  observe({
    invalidateLater(500, session)
    input$AantalVacatures_SelectImp
    input$redraw
    if (isolate(reac$redraw)) {
      reac$selections <- input$AantalVacatures_SelectImp
    } else {
      isolate(reac$redraw <- TRUE)
    }
  })
}