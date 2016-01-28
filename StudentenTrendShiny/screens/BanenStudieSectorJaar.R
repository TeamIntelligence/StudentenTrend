BanenStudieSectorJaarUI <- function(PageName){
  return(
    #vervulde banen per banensector
    tabItem(tabName = PageName,
            fluidRow(
              box(width=12, collapsible = T, title = "Vervulde banen per studiesector per jaar", 
                  p("Op deze pagina vindt u het aantal vervulde banen per studiesector over de periode 2000 tot en met uiterst 2008. De jaartallen doelen op het afstudeermoment van de studenten. Wanneer bijvoorbeeld het peilmoment drie jaar is, is dit voor afstudeerjaar 2005 gemeten in 2008. U kunt zelf kiezen welke studiesector u wilt weergeven. Ook is er een totaallijn van het totaal aantal gediplomeerden studenten te zien. Deze komt uit een andere dataset, hierdoor kunnen er kleine verschillen in de data zitten."),
                  p("De plots die op deze pagina worden getoond geven inzicht voor een student hoe snel hij of zij aan een baan komt. Dit kan je dan makkelijk zien ten opzichte van het totaal aantal gediplomeerden binnen deze studiesector. Het valt ons op dat er een groot aantal studenten direct aan een baan komt. Het verschil tussen direct en binnen een jaar na afstuderen aan een baan komen is relatief hoog. We zien dat er haast geen verschil is tussen aantal banen dat binnen een, twee en drie jaar na afstuderen worden vervuld. Hieruit is te concluderen dat wanneer je binnen een jaar na afstuderen geen baan hebt, je waarschijnlijk binnen drie jaar na afstuderen nog steeds geen baan zal vervullen.")
              ),
              box(width=12, height = 150, 
                  selectInput("BanenStudieSectorJaar_SelectImp",
                              "Selecteer een studiesector om weer te geven:",
                              choices = list(Studiesectoren = unique(gediplomeerden_vacatures$soiCode.soiNaam), "Selecteer een studiesector" = ""),
                              multiple = FALSE,
                              selectize = TRUE,
                              selected=1 
                  )
              ),
              tabBox(width=12, height=550, 
                     tabPanel("Huidige data",
                              box(width=12, height = 470, plotOutput("BanenStudieSectorJaarPlot", height = 450))
                     ),
                     tabPanel("Voorspelling",
                              box(width=12, plotOutput("BanenStudieSectorJaarVoorspellingPlot", height = 450))
                     )
              )
            )   
    )
  )
}

BanenStudieSectorJaarServer <- function(input, output, session){
  
  output$BanenStudieSectorJaarPlot <- renderPlot({
    plotCalcs <- BanenStudieSectorJaarPlotCalcs(input)
    bsjSub <<- plotCalcs$bsjSub
    aantalgediplomeerden <<- plotCalcs$aantalgediplomeerden

    ggplot(bsjSub,   
           aes(x=jaartal)) + 
      xlab("Afstudeerjaar") +  
      ylab("Aantal vervulde banen") + 
      ggtitle("Vervulde banen per studiesector per peilmoment") +
      geom_line(data=bsjSub, aes(y=direct,color="Red")) + 
      geom_point(data=bsjSub,aes(y=direct,color="Red")) + 
      geom_line(data=bsjSub, aes(y=binnenEenJaar,color="Green")) + 
      geom_point(data=bsjSub,aes(y=binnenEenJaar,color="Green")) +
      geom_line(data=bsjSub, aes(y=binnenTweeJaar,color="Blue")) + 
      geom_point(data=bsjSub,aes(y=binnenTweeJaar,color="Blue")) +
      geom_line(data=bsjSub, aes(y=binnenDrieJaar,color="Purple")) + 
      geom_point(data=bsjSub,aes(y=binnenDrieJaar,color="Purple")) +
      geom_line(data=aantalgediplomeerden, aes(y=aantal,color="Black")) + 
      geom_point(data=aantalgediplomeerden,aes(y=aantal,color="Black")) +
      scale_color_manual(values = c("Red","Green","Blue","Purple","Black"), 
                         labels=c("Direct","Binnen een jaar","Binnen twee jaar","Binnen drie jaar", "Aantal gediplomeerden"), 
                         breaks = c("Red","Green","Blue","Purple","Black"), name = "Peilmoment") +
      xlim(2000,2008)
  })
  
  output$BanenStudieSectorJaarVoorspellingPlot <- renderPlot({
    plotCalcs               <- BanenStudieSectorJaarPlotCalcs(input)
    bsjSub                  <<- plotCalcs$bsjSub
    aantalgediplomeerden    <- plotCalcs$aantalgediplomeerden
    
    minYear                 <- bsjSub$jaartal[which.min(bsjSub$jaartal)]
    maxYear                 <- bsjSub$jaartal[which.max(bsjSub$jaartal)]
    
    bSJForeCastSubDirect                <<- createForecastSub(bsjSub[,c("jaartal", "direct")], "direct", "singleColumn", minYear, maxYear, "",DF = 1)
    bSJForeCastSubDirect$group          = "Direct"
    bSJForeCastSubBinnenEenJaar         <<- createForecastSub(bsjSub[,c("jaartal", "binnenEenJaar")], "binnenEenJaar", "singleColumn", minYear, maxYear-1, "",DF = 1)
    bSJForeCastSubBinnenEenJaar$group   = "Binnen 1 jaar"
    bSJForeCastSubBinnenTweeJaar        <<- createForecastSub(bsjSub[,c("jaartal", "binnenTweeJaar")], "binnenTweeJaar", "singleColumn", minYear, maxYear-2, "",DF = 1)
    bSJForeCastSubBinnenTweeJaar$group  = "Binnen 2 jaar"
    bSJForeCastSubbinnenDrieJaar        <<- createForecastSub(bsjSub[,c("jaartal", "binnenDrieJaar")], "binnenDrieJaar", "singleColumn", minYear, maxYear-3, "",DF = 1)
    bSJForeCastSubbinnenDrieJaar$group  = "Binnen 3 jaar"
    bSJForeCastTotaal                   <<- createForecastSub(aantalgediplomeerden[which(aantalgediplomeerden$jaartal %in% c(minYear:maxYear)),], "aantal", "singleColumn", minYear, maxYear, "",DF = 1)
    bSJForeCastTotaal$group             = "Aantal gediplomeerden" 
    
    ggplot(bSJForeCastTotaal,   
           aes(x=jaartal)) + 
      xlab("Afstudeerjaar") +  
      ylab("Aantal vervulde banen") + 
      ggtitle("Vervulde banen per studiesector per peilmoment") +
      geom_line(data=bSJForeCastSubDirect, aes(y=direct,color="Red", group=group)) + 
      geom_point(data=bSJForeCastSubDirect, aes(y=direct,color="Red", group=group)) + 
      geom_line(data=bSJForeCastSubDirect, linetype="dashed", size=1, aes(y=fitted, color="Red", group=group)) +
      
      geom_line(data=bSJForeCastSubBinnenEenJaar, aes(y=binnenEenJaar,color="Green", group=group)) + 
      geom_point(data=bSJForeCastSubBinnenEenJaar,aes(y=binnenEenJaar,color="Green", group=group)) +
      geom_line(data=bSJForeCastSubBinnenEenJaar, linetype="dashed", size=1, aes(y=fitted, color="Green", group=group)) +
      
      geom_line(data=bSJForeCastSubBinnenTweeJaar, aes(y=binnenTweeJaar,color="Blue", group=group)) + 
      geom_point(data=bSJForeCastSubBinnenTweeJaar,aes(y=binnenTweeJaar,color="Blue", group=group)) +
      geom_line(data=bSJForeCastSubBinnenTweeJaar, linetype="dashed", size=1, aes(y=fitted, color="Blue", group=group)) +
      
      geom_line(data=bSJForeCastSubbinnenDrieJaar, aes(y=binnenDrieJaar,color="Purple", group=group)) + 
      geom_point(data=bSJForeCastSubbinnenDrieJaar, aes(y=binnenDrieJaar,color="Purple", group=group)) +
      geom_line(data=bSJForeCastSubbinnenDrieJaar, linetype="dashed", size=1, aes(y=fitted, color="Purple", group=group)) +
      
      geom_line(data=bSJForeCastTotaal, aes(y=aantal,color="Black", group=group)) + 
      geom_point(data=bSJForeCastTotaal,aes(y=aantal,color="Black", group=group)) +
      geom_line(data=bSJForeCastTotaal, linetype="dashed", size=1, aes(y=fitted, color="Black", group=group)) +
      geom_ribbon(data=bSJForeCastTotaal, aes(ymin=lo80, ymax=hi80, x=jaartal, group=group), fill="red", alpha=.25) +
      geom_ribbon(data=bSJForeCastTotaal, aes(ymin=lo95, ymax=hi95, x=jaartal, group=group), fill="darkred", alpha=.25) +
      
      scale_color_manual(values = c("Red","Green","Blue","Purple","Black"), 
                         labels=c("Direct","Binnen een jaar","Binnen twee jaar","Binnen drie jaar", "Aantal gediplomeerden"), 
                         breaks = c("Red","Green","Blue","Purple","Black"), name = "Peilmoment")
#       xlim(2000,2008)
    
    
  })
  
  BanenStudieSectorJaarPlotCalcs <- function(input) {
    
    #data aanpassen nav keuze gebruiker: SOI Studielijntjes
    bsjSub <- gediplomeerden_vacatures[gediplomeerden_vacatures$soiCode.soiNaam %in% input$BanenStudieSectorJaar_SelectImp,]
    #data aanpassen: HBO en WO optellen 
    bsjSub <- aggregate(cbind(bsjSub$direct,bsjSub$binnenEenJaar,bsjSub$binnenTweeJaar,bsjSub$binnenDrieJaar), by=list(jaartal =bsjSub$jaartal), FUN=sum)
    colnames(bsjSub)<-c("jaartal","direct","binnenEenJaar","binnenTweeJaar","binnenDrieJaar")
    
    # gekozen soi omzetten naar isced
    gekozenisced <- soicodes[soicodes$soiNaam %in% input$BanenStudieSectorJaar_SelectImp,]$iscedCodes[[1]]
    #gekozenisced <- soicodes[soicodes$soiNaam %in% "Leraren",]$iscedCodes[[1]]
    
    # aantal gediplomeerden omzetten naar gekozen soi
    aantalgediplomeerden <- studenten_gediplomeerden[studenten_gediplomeerden$iscedCode.iscedCode %in% gekozenisced$iscedCode, ]
    aantalgediplomeerden <- aantalgediplomeerden[(aantalgediplomeerden$ondCode == "HBO" & aantalgediplomeerden$diploma == "Bachelor") | (aantalgediplomeerden$ondCode == "WO" & aantalgediplomeerden$diploma == "Wo-master"),] 
    aantalgediplomeerden <- aggregate(aantalgediplomeerden$aantal, by=list(jaartal=aantalgediplomeerden$jaartal), FUN=sum)
    colnames(aantalgediplomeerden) <- c("jaartal","aantal")
    
    
    return(
      list(bsjSub = bsjSub, aantalgediplomeerden=aantalgediplomeerden)
    )
  }
  
  
}