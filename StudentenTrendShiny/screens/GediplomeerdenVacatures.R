GediplomeerdenVacaturesUI <- function(PageName){
  return(
    tabItem(tabName = PageName,
            fluidRow(
              box(width=12, collapsible = T, title = "Vervulde banen per studie- per bedrijfssector", 
                  p("Op deze pagina vindt u het aantal vervulde banen per studie- per bedrijfssector over de periode 2000 tot en met uiterst 2008. De jaartallen doelen op het afstudeermoment van de studenten. Wanneer bijvoorbeeld het peilmoment op drie jaar wordt gezet, is dit voor afstudeerjaar 2005 gemeten in 2008. U kunt zelf kiezen welke studiesector en welk peilmoment u wilt weergeven. Deze bedrijven worden gegroepeerd per bedrijfssector. Er wordt ook een totaallijn weergegeven. Deze omvat het totaal aantal vervulde banen binnen deze studiesector."),
                  p("De plots die op deze pagina worden getoond geven inzicht voor een student hoe snel hij of zij aan een baan komt binnen een bepaalde bedrijfssector. Het valt ons op dat het bij veel studiesectoren nog niet vast staat binnen welke bedrijfssector de student uiteindelijk zal gaan werken. Met als uitzondering gezondheidszorg en het onderwijs.")
              ),
              box(width=5, height = 150, 
                  
                  selectInput("GediplomeerdenVacatures_SelectImp",
                              "Selecteer een studiesector om weer te geven:",
                              choices = list(Studiesectoren = unique(gediplomeerden_vacatures$soiCode.soiNaam), "Selecteer een studiesector" = ""),
                              multiple = FALSE,
                              selectize = TRUE,
                              selected=1 
                  )
              ),
              box(width=7, height=150,
                  sliderInput("GediplomeerdenVacatures_yearRangeSlider","Peilmoment", min = 0, max = 3, value = 0)
              ),
              tabBox(width=12, height=550, 
                     tabPanel("Huidige data",
                              box(width=5, height = 470, plotlyOutput("GedipVacaPlot", height = 450)),
                              box(width=7, height = 470, plotOutput("GedipVacaBarPlot", height=450))
                     ),
                     tabPanel("Voorspelling",
                              box(width=12, plotOutput("GedipVacaVoorspellingPlot", height = 450))
                     )
              )
            )   
    )
  )
}

GediplomeerdenVacaturesServer <- function(input, output, session){
  
  #######################
  ## NORMALE LINE PLOT ##
  #######################
  output$GedipVacaPlot <- renderPlotly({
    plotCalcs <- GediplomeerdenVacaturesPlotCalc(input)

    plot <- ggplot(plotCalcs$gvSub, aes(x=jaartal)) + 
      xlab("Afstudeerjaar") +  
      ylab("Aantal vervulde banen") + 
      ggtitle("Vervulde banen per studie- per bedrijfssector") +
      theme(legend.position="none") +
      geom_line(data=plotCalcs$gvSub, aes(y=aantal,     #lijnen studies
                                          group=soort,
                                          color=soort), size=-1) + 
      geom_point(data=plotCalcs$gvSub,aes(y=aantal, 
                                          group=soort,
                                          color=soort), size=-1) +
      xlim(2000,plotCalcs$toYear) +
      scale_color_manual(values=GetColors(plotCalcs$gvSub$soort), labels=unique(plotCalcs$gvSub$soort))
    
    PrintGGPlotly(plot)
  })
  
  ######################
  ## NORMALE BAR PLOT ##
  ######################
  output$GedipVacaBarPlot <- renderPlot({
    plotCalcs <- GediplomeerdenVacaturesPlotCalc(input, isBar=TRUE)
    
    plot <- ggplot(plotCalcs$gvSub, aes(x=jaartal)) + 
      xlab("Afstudeerjaar") +  
      ylab("Aantal vervulde banen") + 
      ggtitle("Vervulde banen per studie- per bedrijfssector") +
      geom_bar(stat = "identity", aes(y=aantal, fill=sbiCode93.sbiNaam93))+
      scale_fill_manual(values=GetColors(plotCalcs$gvSub$sbiCode93.sbiNaam93),name="Bedrijfssector") +
      labs(color = "Totaallijn")+
      xlim(1999,plotCalcs$toYear+1)
    
    AddTotaalLines(plot=plot, data=plotCalcs$gvSub)
  })
  
  #########################
  ## VOORSPELLINGEN PLOT ##
  #########################
  output$GedipVacaVoorspellingPlot <- renderPlot({
    plotCalcs <- GediplomeerdenVacaturesPlotCalc(input)
    minYear   <- plotCalcs$gvSub$jaartal[which.min(plotCalcs$gvSub$jaartal)]
    maxYear   <- plotCalcs$toYear
    
    for(val in unique(plotCalcs$gvSub$soiCode.soiNaam)){
      removal_candidates <- plotCalcs$gvSub[plotCalcs$gvSub$sbiCode93.sbiNaam93 == val,]
      allZero = T
      for(row in removal_candidates$aantal){
        if(!is.na(row)){
          if(row != 0){
            allZero = F
          }
        }
      }
      if(allZero){
        plotCalcs$gvSub <- plotCalcs$gvSub[setdiff(rownames(plotCalcs$gvSub),rownames(removal_candidates)),]
      }
    } 
    
    GVVForeCastSub          <- createForecastSub(plotCalcs$gvSub, "aantal", "soort", minYear, maxYear, "",DF = 1)
    
    plot <- ggplot(GVVForeCastSub, aes(x=jaartal)) + 
      xlab("Afstudeerjaar") +  
      ylab("Aantal vervulde banen") + 
      ggtitle("Vervulde banen per studie- per bedrijfssector")
    
    AddTotaalLines(plot=plot, data=GVVForeCastSub, forecast=T, name="Bedrijfssector")
  })
  
  
}

GediplomeerdenVacaturesPlotCalc <- function(input, isBar=FALSE) {
  #data aanpassen nav keuze gebruiker: SBI Studielijntjes
  gvSub <- gediplomeerden_vacatures[gediplomeerden_vacatures$soiCode.soiNaam %in% input$GediplomeerdenVacatures_SelectImp, ]
  
  if(input$GediplomeerdenVacatures_yearRangeSlider == 0){
    columnNames <- gvSub$direct
    toYear <- 2008
  } else if(input$GediplomeerdenVacatures_yearRangeSlider == 1) {
    columnNames <- gvSub$binnenEenJaar
    toYear <- 2007
  } else if(input$GediplomeerdenVacatures_yearRangeSlider == 2) {
    columnNames <- gvSub$binnenTweeJaar
    toYear <- 2006
  } else if(input$GediplomeerdenVacatures_yearRangeSlider == 3) {
    columnNames <- gvSub$binnenDrieJaar
    toYear <- 2005
  }
  
  #data aanpassen: HBO en WO optellen voor biinnen3jaar
  gvSub <- aggregate(columnNames, by=list(jaartal =gvSub$jaartal,sbiCode93.sbiNaam93=gvSub$sbiCode93.sbiNaam93), FUN=sum)
  
  if(isBar) {
    colnames(gvSub)<-c("jaartal","sbiCode93.sbiNaam93","aantal")
  } else {
    colnames(gvSub)<-c("jaartal","soort","aantal")
  }
  
  #Totaal berekenen aantal studenten
  gvSub <- TotaalAantal(data = gvSub,
                        subSet = gvSub,
                        filterParams = c("jaartal"))
  
  return(
    list(gvSub = gvSub, toYear=toYear)
  )
}