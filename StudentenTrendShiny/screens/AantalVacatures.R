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
                        choices = vacatures$sbiCode.sbiNaam,
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
            box(width=5,plotOutput("VacaPlot", height = 450)),
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
  
  #######################
  ## NORMALE LINE PLOT ##
  #######################
  output$VacaPlot <- renderPlot({
    
    #Data aanpassen nav keuze bedrijfssector
    AantalVacatures_vacSub <- vacatures_jaartallen[vacatures_jaartallen$sbiCode.sbiNaam %in% input$AantalVacatures_SelectImp,]
    
    #Totaal set
    totaalaantal <- aggregate(vacatures_jaartallen$aantal, by=list(jaartal=vacatures_jaartallen$jaartal), FUN=sum)
    colnames(totaalaantal)<-c("jaartal","aantal") 
    totaalaantal$soort = "Totale vacatures" 
    
    #Baseplot
    AVLineBaseplot <- ggplot(AantalVacatures_vacSub, aes(x=jaartal)) +
      xlab("Jaar") + 
      ylab("Aantal vacatures") +
      ggtitle("Aantal vacatures per sector") +
      geom_line(data=AantalVacatures_vacSub,
                aes(y=aantal, color=sbiCode.sbiNaam)) +
      geom_point(data=AantalVacatures_vacSub,
                 aes(y=aantal, color=sbiCode.sbiNaam)) +
      scale_color_manual(values=GetColors(AantalVacatures_vacSub$sbiCode.sbiNaam)) +
      theme(legend.position="none")
    
    #Als totaalselect lijn aan staat en er meer als 0 studies geselecteerd zijn
    if (input$AantalVacatures_TotaalSelect == TRUE && length(input$AantalVacatures_SelectImp) != 0) { 
      
      #Totaalselect subset maken
      totaalaantalselect <- aggregate(AantalVacatures_vacSub$aantal, by=list(jaartal=AantalVacatures_vacSub$jaartal), FUN=sum)
      colnames(totaalaantalselect)<-c("jaartal", "aantal")
      totaalaantalselect$soort = "Totale geselecteerde vacatures"
        
      AVLineBaseplot <- AVLineBaseplot + 
        geom_line(data=totaalaantalselect, 
                  aes(y=aantal, group=soort, color=soort),
                  color = "gray48") + 
        geom_point(data=totaalaantalselect, 
                   aes(y=aantal, group=soort, color=soort),
                   color = "gray48")
    }
    
    if (input$AantalVacatures_Totaal == TRUE) {

      AVLineBaseplot <- AVLineBaseplot +
        geom_line(data=totaalaantal, 
                  aes(y=aantal, group=soort, color=soort),
                  color = "black") + 
        geom_point(data=totaalaantal,
                   aes(y=aantal, group=soort, color=soort),
                   color = "black")
     }

     AVLineBaseplot
  })
  
  ######################
  ## NORMALE BAR PLOT ##
  ######################
  output$VacaBarPlot <- renderPlot({
    
    #Data aanpassen nav keuze bedrijfssector
    AantalVacatures_vacBarSub <- vacatures_jaartallen[vacatures_jaartallen$sbiCode.sbiNaam %in% input$AantalVacatures_SelectImp,]
    
    totaalaantal <- aggregate(vacatures_jaartallen$aantal, by=list(jaartal=vacatures_jaartallen$jaartal), FUN=sum)
    colnames(totaalaantal)<-c("jaartal","aantal") 
    totaalaantal$soort = "Totale vacatures" 
    
    AVBarBaseplot <- ggplot(AantalVacatures_vacBarSub, aes(x=jaartal)) +
      xlab("Jaar") + 
      ylab("Aantal vacatures") +
      ggtitle("Aantal vacatures per sector") +
      geom_bar(stat = "identity", aes(y=aantal, fill=sbiCode.sbiNaam)) + 
      scale_fill_manual(values=GetColors(AantalVacatures_vacBarSub$sbiCode.sbiNaam), name = "Bedrijfssector")
    
    #scale_color_manual options
    scmOptionsList.names <- c("values", "breaks", "labels")
    scmOptionsList <- setNames(vector("list", length(scmOptionsList.names )), scmOptionsList.names )
    
    if (input$AantalVacatures_Totaal == TRUE ){
      
      AVBarBaseplot <- AVBarBaseplot +
        geom_line(data=totaalaantal, aes(y=aantal,  #totaal lijn
                                         group=soort,
                                         color= "black")) + 
        geom_point(data=totaalaantal, 
                   aes(y=aantal,  group=soort, color="black")) +
        labs(color = "Totaallijn")
      
      scmOptionsList$values <- c(scmOptionsList$values, "black")
      scmOptionsList$breaks <- c(scmOptionsList$breaks, "black")
      scmOptionsList$labels <- c(scmOptionsList$labels, "Totaallijn")
    } 
    
    if (input$AantalVacatures_TotaalSelect == TRUE && length(input$AantalVacatures_SelectImp) != 0){ 

      totaalaantalselect <- aggregate(AantalVacatures_vacBarSub$aantal, by=list(jaartal=AantalVacatures_vacBarSub$jaartal), FUN=sum)
      colnames(totaalaantalselect)<-c("jaartal", "aantal")
      totaalaantalselect$soort = "Totale geselecteerde vacatures"
      
      AVBarBaseplot <- AVBarBaseplot +
          geom_line(data=totaalaantalselect, 
                    aes(y=aantal, group=soort, color="gray48")) + 
          geom_point(data=totaalaantalselect, 
                     aes(y=aantal, group=soort, color= "gray48")) +
          labs(color = "Totaallijn")
      
      scmOptionsList$values <- c(scmOptionsList$values, "gray48")
      scmOptionsList$breaks <- c(scmOptionsList$breaks, "gray48")
      scmOptionsList$labels <- c(scmOptionsList$labels, "Totaallijn geselecteerde")
    }
    
    AVBarBaseplot +
      scale_color_manual(values=scmOptionsList$values,breaks=scmOptionsList$breaks, labels=scmOptionsList$labels)
  })
  
  #########################
  ## VOORSPELLINGEN PLOT ##
  #########################
  output$VacaVoorspellingPlot <- renderPlot({
    
    #Data aanpassen nav keuze bedrijfssector
    AantalVacatures_vacSub      <- vacatures_jaartallen[vacatures_jaartallen$sbiCode.sbiNaam %in% input$AantalVacatures_SelectImp,]
    aantalVacatures_forecastSub <- createForecastSub(AantalVacatures_vacSub, "aantal", "sbiCode.sbiNaam", 1997, 2014, 2015)

    #Totaal berekenen
    totaalaantal           <- aggregate(vacatures_jaartallen$aantal, by=list(jaartal=vacatures_jaartallen$jaartal), FUN=sum)
    colnames(totaalaantal) <- c("jaartal","aantal") 
    forecastTotaal         <- createForecastSub(totaalaantal, "aantal", "singleColumn", 1997, 2014, 2015)
    forecastTotaal$soort   = "Totale vacatures" 
    
    AVForecastBaseplot <- ggplot(aantalVacatures_forecastSub, aes(x=jaartal)) +
      xlab("Jaar") + 
      ylab("Aantal vacatures") +
      ggtitle("Aantal vacatures per sector") +
      geom_line(linetype="dashed", size=1,
                aes(y=fitted,
                    group=sbiCode.sbiNaam,
                    color=sbiCode.sbiNaam))+
      geom_line(aes(y=aantal, 
                    group=sbiCode.sbiNaam,
                    color=sbiCode.sbiNaam))+
      geom_point(aes(y=aantal, 
                     group=sbiCode.sbiNaam,
                     color=sbiCode.sbiNaam))+
      scale_color_manual(values=GetColors(AantalVacatures_vacSub$sbiCode.sbiNaam), name = "Bedrijfssector")

    if (input$AantalVacatures_Totaal == TRUE ){

      AVForecastBaseplot <- AVForecastBaseplot +
        #TOTAALLIJN
        geom_line(data=forecastTotaal, 
                  aes(y=aantal, group=soort, color=soort),
                  color = "black") + 
        geom_point(data=forecastTotaal, 
                   aes(y=aantal, group=soort, color=soort),
                   color = "black") +
        geom_line(data=forecastTotaal, linetype="dashed", size=1,
                  aes(y=fitted, group=soort, color=soort),
                  color = "black") + 
        geom_ribbon(data=forecastTotaal, aes(ymin=lo80, ymax=hi80, x=jaartal, group=soort), fill="red", alpha=.25) +
        geom_ribbon(data=forecastTotaal, aes(ymin=lo95, ymax=hi95, x=jaartal, group=soort), fill="darkred", alpha=.25)
    } 
    if (input$AantalVacatures_TotaalSelect == TRUE && length(input$AantalVacatures_SelectImp) != 0){
      
      totaalaantalselect <- aggregate(aantalVacatures_forecastSub$aantal, by=list(jaartal=aantalVacatures_forecastSub$jaartal), FUN=sum)
      colnames(totaalaantalselect)<-c("jaartal", "aantal")
      totaalaantalselect <- totaalaantalselect[totaalaantalselect$jaartal != 2015,]
      
      forecastTotaalselect   <- createForecastSub(totaalaantalselect, "aantal", "singleColumn", 1997, 2014, 2015)
      forecastTotaalselect$soort = "Totale geselecteerde vacatures"
      
      AVForecastBaseplot <- AVForecastBaseplot +
        geom_line(data=forecastTotaalselect, 
                  aes(y=aantal, group=soort, color=soort), 
                  color = "gray48") + 
        geom_point(data=forecastTotaalselect,
                   aes(y=aantal, group=soort, color=soort),
                   color = "gray48") +
        geom_line(data=forecastTotaalselect, linetype="dashed", size=1,
                  aes(y=fitted, group=soort, color=soort),
                  color = "gray48") + 
        geom_ribbon(data=forecastTotaalselect, aes(ymin=lo80, ymax=hi80, x=jaartal, group=soort), fill="blue", alpha=.25) +
        geom_ribbon(data=forecastTotaalselect, aes(ymin=lo95, ymax=hi95, x=jaartal, group=soort), fill="darkblue", alpha=.25)
    } 
    
    AVForecastBaseplot
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
}