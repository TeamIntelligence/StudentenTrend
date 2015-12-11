BanenStudieSectorJaarUI <- function(PageName){
  return(
    #vervulde banen per banensector
    tabItem(tabName = PageName,
            # Page title
            titlePanel("Vervulde banen per studiesector per jaar"),
            fluidRow(
              box(width=12, height = 150, 
                  selectInput("BanenStudieSectorJaar_SelectImp",
                              "Selecteer een studiesector om weer te geven:",
                              choices = gediplomeerden_vacatures$soiCode.soiNaam,
                              multiple = FALSE,
                              selectize = TRUE,
                              selected=1 
                  )
              ),
              box(width=12, height = 470, plotOutput("BanenStudieSectorJaarPlot", height = 450))
            )   
    )
  )
}

BanenStudieSectorJaarServer <- function(input, output, session){
  
  output$BanenStudieSectorJaarPlot <- renderPlot({
    
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
    
    
    ggplot(bsjSub,   
           aes(x=jaartal)) + 
      xlab("Jaar") +  
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
                         breaks = c("Red","Green","Blue","Purple","Black")) +
      xlim(2000,2008)+
      labs(color = "Peilmoment")  

    
  })
  
  
}