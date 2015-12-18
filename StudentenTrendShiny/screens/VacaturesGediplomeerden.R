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
                              choices = gediplomeerden_vacatures$sbiCode93.sbiNaam93,
                              multiple = FALSE,
                              selectize = TRUE,
                              selected=1 
                  )
              ),
              box(width=7, height=150, 
                  sliderInput("VacaturesGediplomeerden_yearRangeSlider","Peilmoment", min = 0, max = 3, value = 0)
              )
              ,box(width=5, height = 470, plotOutput("VacaGedipPlot", height = 450))
              ,box(width=7, height = 470, plotOutput("VacaGedipBarPlot", height=450))
            )   
    )
  )
}

VacaturesGediplomeerdenServer <- function(input, output, session){
  
  output$VacaGedipPlot <- renderPlot({

    plotCalcs <- VacaturesGediplomeerdenPlotCalc(input)
    
    #vacatures jaartallen inladen
    # gekozen SBI93 omzetten naar 2008
#     selectie2008 <- sbicodes93[sbicodes93$sbiNaam93 %in% input$VacaturesGediplomeerden_SelectImp,]$sbiCodes[[1]]
#      # aantal vacatures omzetten naar gekozen 
#     aantalvacatures <- vacatures_jaartallen[vacatures_jaartallen$sbiCode.sbiCode %in% selectie2008$sbiCode, ]
#     aantalvacatures <- aggregate(aantalvacatures$aantal, by=list(jaartal=aantalvacatures$jaartal), FUN=sum)
#     colnames(aantalvacatures) <- c("jaartal","aantal")
    
    ggplot(plotCalcs$totaalaantal,   
           aes(x=jaartal)) + 
      xlab("Afstudeerjaar") +  
      ylab("Aantal vervulde banen") + 
      ggtitle("Vervulde banen per bedrijfs- per studiesector") +
      geom_line(data=plotCalcs$vgSub, aes(y=aantal,     #lijnen studies
                                group=soiCode.soiNaam,
                                color=soiCode.soiNaam)) + 
      geom_point(data=plotCalcs$vgSub,aes(y=aantal, 
                                group=soiCode.soiNaam,
                                color=soiCode.soiNaam)) +
      geom_line(data=plotCalcs$totaalaantal, aes(y=aantal),  #totaal lijn
                                        color = "black") + 
      geom_point(data=plotCalcs$totaalaantal, aes(y=aantal), 
                                         color = "black") + 
      scale_color_manual(values=GetColors(plotCalcs$vgSub$soiCode.soiNaam)) +
      theme(legend.position="none") +
      xlim(2000,plotCalcs$toYear)
    
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
  
  return(
    list(vgSub = vgSub, toYear=toYear, totaalaantal=totaalaantal)
  )
}