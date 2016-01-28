VacaturesGediplomeerdenUI <- function(PageName){
  return(
    #vervulde banen per banensector
    tabItem(tabName = PageName,
            fluidRow(
              box(width=12, collapsible = T, title = "Vervulde banen per bedrijfs- per studiesector", 
                  p("Op deze pagina vindt u het aantal vervulde banen per bedrijfs- per studiesector over de periode 2000 tot en met uiterst 2008. De jaartallen van de plots doelen op het afstudeermoment van de studenten. Wanneer bijvoorbeeld het peilmoment op 3 jaar wordt gezet, is dit voor afstudeerjaar 2005 gemeten in 2008. U kunt zelf kiezen welke bedrijfssector en welk peilmoment u wilt weergeven. Deze studenten worden gegroepeerd per studiesector. Er wordt ook een totaallijn weergegeven. Deze omvat het totaal aantal vervulde banen binnen deze bedrijfssector. "),
                  p("De plots die op deze pagina worden getoond geven inzicht voor een bedrijf binnen een bepaalde bedrijfssector van welke opleiding de studenten komen. Bij de onderwijssector hadden we wel verwacht dat er meer studenten van andere opleidingen dan de lerarenopleiding zouden komen. Verder zien we bij sommige bedrijfssectoren de Dot-com bubble wel een beetje terug komen, maar niet bij iedere bedrijfssector.")
              ),
              box(width=5, height = 150, 
                  
                  selectInput("VacaturesGediplomeerden_SelectImp",
                              "Selecteer een bedrijfssector om weer te geven:",
                              choices = list(Bedrijfssectoren = unique(gediplomeerden_vacatures$sbiCode93.sbiNaam93), "Selecteer een bedrijfssector" = ""),
                              multiple = FALSE,
                              selectize = TRUE,
                              selected=1 
                  )
              ),
              box(width=7, height=150, 
                  sliderInput("VacaturesGediplomeerden_yearRangeSlider","Peilmoment", min = 0, max = 3, value = 0)
              ),
              tabBox(width=12, height=550, 
                     tabPanel("Huidige data",
                              box(width=5, height = 470, plotlyOutput("VacaGedipPlot", height = 450)),
                              box(width=7, height = 470, plotOutput("VacaGedipBarPlot", height=450))
                     ),
                     tabPanel("Voorspelling",
                              box(width=12, plotOutput("VacaGedipVoorspellingPlot", height = 450))
                     )
              )
            )   
    )
  )
}

VacaturesGediplomeerdenServer <- function(input, output, session){
  
  output$VacaGedipPlot <- renderPlotly({

    plotCalcs <- VacaturesGediplomeerdenPlotCalc(input)
    scmOptionsList        <- InitGGLegend()
    scmOptionsList$values <- c(scmOptionsList$values, GetColors(plotCalcs$vgSub$soiCode.soiNaam))
    scmOptionsList$labels <- c(scmOptionsList$labels, unique(plotCalcs$vgSub$soiCode.soiNaam))
    
    plot <- ggplot(plotCalcs$totaalaantal,   
                   aes(x=jaartal)) + 
      xlab("Afstudeerjaar") +  
      ylab("Aantal vervulde banen") + 
      ggtitle("Vervulde banen per bedrijfs- per studiesector") +
      geom_line(data=plotCalcs$vgSub, aes(y=aantal,     #lijnen studies
                                          group=soiCode.soiNaam,
                                          color=soiCode.soiNaam), size=-1) + 
      geom_point(data=plotCalcs$vgSub,aes(y=aantal, 
                                          group=soiCode.soiNaam,
                                          color=soiCode.soiNaam), size=-1) +
      theme(legend.position="none") +
      xlim(2000,plotCalcs$toYear)
    
    TotaalLine     <- AddTotaalLine(plot=plot, data=plotCalcs$totaalaantal, colors=scmOptionsList, size=-1)
    plot           <- TotaalLine$plot
    scmOptionsList <- TotaalLine$colors
    
    PrintGGPlotly(plot + 
      scale_color_manual(values=scmOptionsList$values, labels=scmOptionsList$labels)
    )
  })
  
  output$VacaGedipBarPlot <- renderPlot({
    plotCalcs <- VacaturesGediplomeerdenPlotCalc(input)
    
    ggplot(plotCalcs$vgSub, 
           aes(x=jaartal)) + 
      xlab("Afstudeerjaar") +  
      ylab("Aantal vervulde banen") + 
      ggtitle("Vervulde banen per bedrijfs- per studiesector") +
      geom_bar(data = plotCalcs$vgSub, stat = "identity",
               aes(y=aantal, fill=soiCode.soiNaam))+
      geom_line(data=plotCalcs$totaalaantal, aes(y=aantal,  #totaal lijn
                color = "black")) + 
      geom_point(data=plotCalcs$totaalaantal, aes(y=aantal, 
                 color = "black")) + 
      scale_fill_manual(values=GetColors(plotCalcs$vgSub$soiCode.soiNaam),name="Studiesector") +
      scale_color_manual(values=c("black"),breaks=c("black"), labels=c("Totaal aantal vervulde banen"))+
      labs(color = "Totaallijn")+
      xlim(1999,plotCalcs$toYear+1)
  })
  
  output$VacaGedipVoorspellingPlot <- renderPlot({
    plotCalcs <- VacaturesGediplomeerdenPlotCalc(input)
    minYear   <- plotCalcs$vgSub$jaartal[which.min(plotCalcs$vgSub$jaartal)]
    maxYear   <- plotCalcs$toYear
  
    for(val in unique(plotCalcs$vgSub$soiCode.soiNaam)){
      removal_candidates <- plotCalcs$vgSub[plotCalcs$vgSub$soiCode.soiNaam == val,]
      allZero = T
      for(row in removal_candidates$aantal){
        if(!is.na(row)){
          if(row != 0){
            allZero = F
            }
          }
      }
      if(allZero){
        plotCalcs$vgSub <- plotCalcs$vgSub[setdiff(rownames(plotCalcs$vgSub),rownames(removal_candidates)),]
      }
    } 

    VGVForeCastSub          <- createForecastSub(plotCalcs$vgSub, "aantal", "soiCode.soiNaam", minYear, maxYear, "",DF = 1)
    VGVForeCastTotaal       <- createForecastSub(plotCalcs$totaalaantal, "aantal", "singleColumn", minYear, maxYear, "",DF = 1)
    VGVForeCastTotaal$soort = "Totaal" 
    
    scmOptionsList         <- InitGGLegend()
    sfillmanualOptionsList <- InitGGLegend()
    scmOptionsList$values  <- c(scmOptionsList$values, GetColors(VGVForeCastSub$soiCode.soiNaam))
    scmOptionsList$labels  <- c(scmOptionsList$labels, unique(VGVForeCastSub$soiCode.soiNaam))
    
    plot <- ggplot(VGVForeCastSub,   
                   aes(x=jaartal)) + 
      xlab("Afstudeerjaar") +  
      ylab("Aantal vervulde banen") + 
      ggtitle("Vervulde banen per bedrijfs- per studiesector") +
      geom_line(data=VGVForeCastSub, aes(y=aantal,     #lijnen studies
                                         group=soiCode.soiNaam,
                                         color=soiCode.soiNaam)) + 
      geom_point(data=VGVForeCastSub,aes(y=aantal, 
                                         group=soiCode.soiNaam,
                                         color=soiCode.soiNaam)) +
      geom_line(data=VGVForeCastSub, linetype="dashed", size=1, aes(y=fitted,
                                                                    group=soiCode.soiNaam,
                                                                    color=soiCode.soiNaam))
  
    TotaalLine             <- AddTotaalLine(plot=plot, 
                                            data=VGVForeCastTotaal, 
                                            colors=scmOptionsList, 
                                            fills=sfillmanualOptionsList,
                                            forecast = TRUE)
    plot                   <- TotaalLine$plot
    scmOptionsList         <- TotaalLine$colors
    sfillmanualOptionsList <- TotaalLine$fills
    
    plot +
      scale_color_manual(values=scmOptionsList$values, labels=scmOptionsList$labels, name="Studiesector") +
      scale_fill_manual(values=sfillmanualOptionsList$values, labels=sfillmanualOptionsList$labels, name="Betrouwbaarheidsinterval")
  })
}

VacaturesGediplomeerdenPlotCalc <- function(input) {
  #data aanpassen nav keuze gebruiker: SBI Studielijntjes
  vgSub <- gediplomeerden_vacatures[gediplomeerden_vacatures$sbiCode93.sbiNaam93 %in% input$VacaturesGediplomeerden_SelectImp, ]
  
  if(input$VacaturesGediplomeerden_yearRangeSlider == 0){
    columnNames <- vgSub$direct
    toYear <- 2008
  } else if(input$VacaturesGediplomeerden_yearRangeSlider == 1) {
    columnNames <- vgSub$binnenEenJaar
    toYear <- 2007
  } else if(input$VacaturesGediplomeerden_yearRangeSlider == 2) {
    columnNames <- vgSub$binnenTweeJaar
    toYear <- 2006
  } else if(input$VacaturesGediplomeerden_yearRangeSlider == 3) {
    columnNames <- vgSub$binnenDrieJaar
    toYear <- 2005
  }
  
  #data aanpassen: HBO en WO optellen voor biinnen3jaar
  vgSub <- aggregate(columnNames, by=list(jaartal =vgSub$jaartal,soiCode.soiNaam=vgSub$soiCode.soiNaam), FUN=sum)
  colnames(vgSub)<-c("jaartal","soiCode.soiNaam","aantal")
  
  #Totaal berekenen aantal studenten
  totaalaantal <- aggregate(vgSub$aantal, by=list(jaartal=vgSub$jaartal), FUN=sum)
  colnames(totaalaantal)<-c("jaartal","aantal")
  totaalaantal$soort <- "Totaal aantal"
  
  return(
    list(vgSub = vgSub, toYear=toYear, totaalaantal=totaalaantal)
  )
}