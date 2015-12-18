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
                              choices = gediplomeerden_vacatures$soiCode.soiNaam,
                              multiple = FALSE,
                              selectize = TRUE,
                              selected=1 
                  )
              ),
              box(width=7, height=150,
                  sliderInput("GediplomeerdenVacatures_yearRangeSlider","Peilmoment", min = 0, max = 3, value = 0)
              )
              ,box(width=5, height = 470, plotOutput("GedipVacaPlot", height = 450))
              ,box(width=7, height = 470, plotOutput("GedipVacaBarPlot", height=450))
            )   
    )
  )
}

GediplomeerdenVacaturesServer <- function(input, output, session){
  
  output$GedipVacaPlot <- renderPlot({
    plotCalcs <- GediplomeerdenVacaturesPlotCalc(input)

    ggplot(plotCalcs$totaalaantal,   
           aes(x=jaartal)) + 
      xlab("Afstudeerjaar") +  
      ylab("Aantal vervulde banen") + 
      ggtitle("Vervulde banen per studie- per bedrijfssector") +
      geom_line(data=plotCalcs$gvSub, aes(y=aantal,     #lijnen studies
                                          group=sbiCode93.sbiNaam93,
                                          color=sbiCode93.sbiNaam93)) + 
      geom_point(data=plotCalcs$gvSub,aes(y=aantal, 
                                          group=sbiCode93.sbiNaam93,
                                          color=sbiCode93.sbiNaam93)) +
      geom_line(data=plotCalcs$totaalaantal, aes(y=aantal),  #totaal lijn
                color = "black") + 
      geom_point(data=plotCalcs$totaalaantal, aes(y=aantal), 
                 color = "black") + 
      scale_color_manual(values=GetColors(plotCalcs$gvSub$sbiCode93.sbiNaam93)) +
      theme(legend.position="none") +
      xlim(2000,plotCalcs$toYear)
  })
  
  output$GedipVacaBarPlot <- renderPlot({
    plotCalcs <- GediplomeerdenVacaturesPlotCalc(input)
    
    ggplot(plotCalcs$gvSub, 
           aes(x=jaartal)) + 
      xlab("Afstudeerjaar") +  
      ylab("Aantal vervulde banen") + 
      ggtitle("Vervulde banen per studie- per bedrijfssector") +
      geom_bar(data = plotCalcs$gvSub, stat = "identity",
               aes(y=aantal, fill=sbiCode93.sbiNaam93))+
      geom_line(data=plotCalcs$totaalaantal, aes(y=aantal,  #totaal lijn
                                                 color = "black")) + 
      geom_point(data=plotCalcs$totaalaantal, aes(y=aantal, 
                                                  color = "black")) + 
      scale_fill_manual(values=GetColors(plotCalcs$gvSub$sbiCode93.sbiNaam93),name="Bedrijfssector") +
      scale_color_manual(values=c("black"),breaks=c("black"), labels=c("Totaal aantal vervulde banen"))+
      labs(color = "Totaallijn")+
      xlim(1999,plotCalcs$toYear+1)
  })
  
  
}

GediplomeerdenVacaturesPlotCalc <- function(input) {
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
  colnames(gvSub)<-c("jaartal","sbiCode93.sbiNaam93","aantal")
  
  #Totaal berekenen aantal studenten
  totaalaantal <- aggregate(gvSub$aantal, by=list(jaartal=gvSub$jaartal), FUN=sum)
  colnames(totaalaantal)<-c("jaartal","aantal")
  
  return(
    list(gvSub = gvSub, toYear=toYear, totaalaantal=totaalaantal)
  )
}