StudentenGediplomeerdenUI <- function(PageName){
  return(
    
    tabItem(tabName = PageName, 
            # Page title
            fluidRow(
              box(width = 12, collapsible = T, title = "Gediplomeerde studenten", 
                  p("Op deze pagina vindt u het aantal gediplomeerde studenten per studiesector over de periode 1995 tot en met 2013. U kunt zelf kiezen welke studiesectoren u wilt weergeven. Daarnaast kunt u ook zelf een keuze maken tussen HBO en WO. Verder kan u ook een totaallijn weergeven van alle studies of een totaallijn van de studies die u geselecteerd hebt."),
                  p("De grafiek biedt inzicht hoeveel studenten elk jaar afstuderen binnen een bepaalde studiesector. Er kan vervolgens uit opgemaakt worden of er een tekort of een teveel zal zijn aan studenten binnen een bepaalde studiesector voor in het bedrijfsleven.")
              ),
              box(width=4, height = 170,
                  selectInput("StudentenGediplomeerden_SelectStudyImp",
                              "Selecteer een of meerdere studiesectoren om weer te geven:",
                              choices = studenten_gediplomeerden$iscedCode.iscedNaam,
                              multiple = TRUE,
                              selectize = TRUE
                  ),
                  checkboxInput("StudentenGediplomeerden_AlleStudies",
                                "Selecteer alle studies"
                  ),
                  #Hoogte handmatig aanpassen van alle select inputs
                  tags$style(type='text/css',".selectize-input { height: 59px; overflow: auto;}")
              ),
              box(width=4, height = 170,
                  radioButtons("StudentenGediplomeerden_StudieNiveau",
                               "Studie Niveau", 
                               choices = list("HBO" = "HBO", 
                                              "WO Bachelor"  = "WOB",
                                              "WO Master" = "WOM",
                                              "HBO en WO Master" = "HBOWO")
                  )
              ),
              box(width=4, height = 170,
                  checkboxInput("StudentenGediplomeerden_TotaalSelect",
                                "Totaal lijn weergeven van de geselecteerde studies"
                  ),
                  checkboxInput("StudentenGediplomeerden_Totaal",
                                "Totaal lijn weergeven"
                  )
              ),
              
              tabBox(width=12, height=550, 
                     tabPanel("Huidige data",
                              box(width=5, plotlyOutput("DiploPlot", height=450)),
                              box(width=7, plotOutput("DiploBarPlot", height=450))
                     ),
                     tabPanel("Voorspelling",
                              box(width=12,plotOutput("DiploVoorspellingPlot", height = 450))
                     )
              )
            )
    )
  )
}

StudentenGediplomeerdenServer <- function(input, output, session){
  
  #######################
  ## NORMALE LINE PLOT ##
  #######################
  output$DiploPlot <- renderPlotly({
    #HBO BACH en WO MAST
    HWSet <- studenten_gediplomeerden[(studenten_gediplomeerden$ondCode == "HBO" & studenten_gediplomeerden$diploma == "Bachelor") | (studenten_gediplomeerden$ondCode == "WO" & studenten_gediplomeerden$diploma == "Wo-master"),] 
    
    #data aanpassen nav keuzes gebruiker
    StudentenGediplomeerden_StudieSub <- switch (input$StudentenGediplomeerden_StudieNiveau,
                                                 "HBO" = studenten_gediplomeerden[studenten_gediplomeerden$ondCode == "HBO" & studenten_gediplomeerden$diploma == "Bachelor",] ,
                                                 "WOB" = studenten_gediplomeerden[studenten_gediplomeerden$ondCode == "WO" & studenten_gediplomeerden$diploma == "Bachelor",],
                                                 "WOM"= studenten_gediplomeerden[studenten_gediplomeerden$ondCode == "WO" & studenten_gediplomeerden$diploma == "Wo-master",],
                                                 "HBOWO" = aggregate(HWSet$aantal, by=list(iscedNaam=HWSet$iscedCode.iscedNaam, jaartal=HWSet$jaartal), FUN=sum)
    )
    
    #namen kolomtitels van de nieuwe gevormde data aanpassen
    if(input$StudentenGediplomeerden_StudieNiveau == "HBOWO"){
      colnames(StudentenGediplomeerden_StudieSub)<-c("iscedCode.iscedNaam","jaartal","aantal")
    }
    
    StudentenGediplomeerden_StudieSub <- StudentenGediplomeerden_StudieSub[StudentenGediplomeerden_StudieSub$iscedCode.iscedNaam %in% input$StudentenGediplomeerden_SelectStudyImp,]
    PlotTitle <- "Aantal gediplomeerde studenten \nper jaar verdeeld per studie"
    
    if (input$StudentenGediplomeerden_StudieNiveau == "HBOWO"){
      data <- HWSet
    } else {
      data <- studenten_gediplomeerden
    }
    
    #totaallijn
    totaalaantal <- TotaalAantal(data = data,
                                 studieNiveauInput = input$StudentenGediplomeerden_StudieNiveau, 
                                 filterParams= c("ondCode",'jaartal',"diploma"))
    
    SGLineBaseplot <- ggplot(StudentenGediplomeerden_StudieSub, aes(x=jaartal)) + 
      xlab("Jaar") +  
      ylab("Aantal studenten") + 
      ggtitle(PlotTitle) +
      geom_line(data=StudentenGediplomeerden_StudieSub, size = -1,
                aes(y=aantal, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam)) + 
      geom_point(data=StudentenGediplomeerden_StudieSub,
                 aes(y=aantal, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam)) +
      scale_color_manual(values=GetColors(StudentenGediplomeerden_StudieSub$iscedCode.iscedNaam)) +
      theme(legend.position="none")
    
    #Totaal lijn toevoegen
    if (input$StudentenGediplomeerden_Totaal == TRUE ){
      SGLineBaseplot <- SGLineBaseplot +
        geom_line(data=totaalaantal, size = -1,
                  aes(y=aantal, group=ondCode, color=ondCode), 
                  color = "black") + 
        geom_point(data=totaalaantal, 
                   aes(y=aantal, group=ondCode, color=ondCode),
                   color = "black")
    }
    
    #alleen select
    if (input$StudentenGediplomeerden_TotaalSelect == TRUE && length(input$StudentenGediplomeerden_SelectStudyImp) != 0){
      totaalaantalselect <- TotaalAantalSelect(data = data,
                                               selectInput = input$StudentenGediplomeerden_SelectStudyImp,
                                               studieNiveauInput = input$StudentenGediplomeerden_StudieNiveau,
                                               filterParams= c("ondCode",'jaartal',"diploma"))
      
      SGLineBaseplot <- SGLineBaseplot +
        geom_line(data=totaalaantalselect, size = -1, 
                  aes(y=aantal, group=ondCode, color=ondCode),
                  color = "gray48") + 
        geom_point(data=totaalaantalselect,
                   aes(y=aantal, group=ondCode, color=ondCode),
                   color = "gray48")
    }
    
    #Render de plot
    if( length(input$StudentenGediplomeerden_SelectStudyImp) != 0) {
      PrintGGPlotly(SGLineBaseplot)
    } else {
      return(SGLineBaseplot)
    }
  })
  
  ######################
  ## NORMALE BAR PLOT ##
  ######################
  output$DiploBarPlot <- renderPlot({
    
    HWBarSet <<- studenten_gediplomeerden[(studenten_gediplomeerden$ondCode == "HBO" & studenten_gediplomeerden$diploma == "Bachelor") | (studenten_gediplomeerden$ondCode == "WO" & studenten_gediplomeerden$diploma == "Wo-master") ,] 
    
    #data aanpassen nav keuzes gebruiker
    StudentenGediplomeerden_StudieBarSub <- switch (input$StudentenGediplomeerden_StudieNiveau,
                                                    "HBO" = studenten_gediplomeerden[studenten_gediplomeerden$ondCode == "HBO" & studenten_gediplomeerden$diploma == "Bachelor",] ,
                                                    "WOB" = studenten_gediplomeerden[studenten_gediplomeerden$ondCode == "WO" & studenten_gediplomeerden$diploma == "Bachelor",],
                                                    "WOM"= studenten_gediplomeerden[studenten_gediplomeerden$ondCode == "WO" & studenten_gediplomeerden$diploma == "Wo-master",],
                                                    "HBOWO" = aggregate(HWBarSet$aantal, by=list(iscedNaam=HWBarSet$iscedCode.iscedNaam, jaartal=HWBarSet$jaartal), FUN=sum)
    )
    
    #namen kolomtitels van de nieuwe gevormde data aanpassen
    if(input$StudentenGediplomeerden_StudieNiveau == "HBOWO"){
      colnames(StudentenGediplomeerden_StudieBarSub)<-c("iscedCode.iscedNaam","jaartal","aantal")
    }
    StudentenGediplomeerden_StudieBarSub <- StudentenGediplomeerden_StudieBarSub[StudentenGediplomeerden_StudieBarSub$iscedCode.iscedNaam %in% input$StudentenGediplomeerden_SelectStudyImp,]
    
    if (input$StudentenGediplomeerden_StudieNiveau == "HBOWO"){
      data <- HWBarSet
    } else {
      data <- studenten_gediplomeerden
    }
    
    
    SGBarBaseplot <-  ggplot(StudentenGediplomeerden_StudieBarSub, aes(x=jaartal)) + 
      xlab("Jaar") +  
      ylab("Aantal studenten") + 
      ggtitle("Aantal gediplomeerde studenten") +
      geom_bar(stat = "identity", aes(y=aantal, fill=iscedCode.iscedNaam)) + 
      scale_fill_manual(values=GetColors(StudentenGediplomeerden_StudieBarSub$iscedCode.iscedNaam), name= "Studierichting")
    
    #scale_color_manual options
    scmOptionsList.names <- c("values", "breaks", "labels")
    scmOptionsList <- setNames(vector("list", length(scmOptionsList.names )), scmOptionsList.names)
    
    if (input$StudentenGediplomeerden_Totaal == TRUE ){
      totaalaantal <- TotaalAantal(data = data, 
                                   studieNiveauInput = input$StudentenGediplomeerden_StudieNiveau, 
                                   filterParams= c("ondCode",'jaartal',"diploma"))
      
      SGBarBaseplot <- SGBarBaseplot +
        geom_line(data=totaalaantal, 
                  aes(y=aantal, group=ondCode, color= "black")) + 
        geom_point(data=totaalaantal, 
                   aes(y=aantal, group=ondCode, color="black")) +
        labs(color = "Totaallijn")
      
      scmOptionsList$values <- c(scmOptionsList$values, "black")
      scmOptionsList$breaks <- c(scmOptionsList$breaks, "black")
      scmOptionsList$labels <- c(scmOptionsList$labels, "Totaallijn")
    }
    
    if (input$StudentenGediplomeerden_TotaalSelect == TRUE && length(input$StudentenGediplomeerden_SelectStudyImp) != 0){
      
      totaalaantalselect <- TotaalAantalSelect(data = data,
                                               selectInput = input$StudentenGediplomeerden_SelectStudyImp,
                                               studieNiveauInput = input$StudentenGediplomeerden_StudieNiveau,
                                               filterParams= c("ondCode",'jaartal',"diploma"))
      
      SGBarBaseplot <- SGBarBaseplot +
        geom_line(data=totaalaantalselect, 
                  aes(y=aantal, group=ondCode, color= "gray48")) + 
        geom_point(data=totaalaantalselect,
                   aes(y=aantal, group=ondCode, color="gray48")) +
        labs(color = "Totaallijn")
      
      scmOptionsList$values <- c(scmOptionsList$values, "gray48")
      scmOptionsList$breaks <- c(scmOptionsList$breaks, "gray48")
      scmOptionsList$labels <- c(scmOptionsList$labels, "Totaallijn geselecteerde")
    }
    
    #Render de plot
    SGBarBaseplot +
      scale_color_manual(values=scmOptionsList$values, breaks=scmOptionsList$breaks, labels=scmOptionsList$labels)
  })
  
  #########################
  ## VOORSPELLINGEN PLOT ##
  #########################
  output$DiploVoorspellingPlot <- renderPlot({
    #HBO BACH en WO MAST
    HWSet <- studenten_gediplomeerden[(studenten_gediplomeerden$ondCode == "HBO" & studenten_gediplomeerden$diploma == "Bachelor") | (studenten_gediplomeerden$ondCode == "WO" & studenten_gediplomeerden$diploma == "Wo-master"),] 
    
    #data aanpassen nav keuzes gebruiker
    StudentenGediplomeerden_StudieSub <- switch (input$StudentenGediplomeerden_StudieNiveau,
                                                 "HBO" = studenten_gediplomeerden[studenten_gediplomeerden$ondCode == "HBO" & studenten_gediplomeerden$diploma == "Bachelor",] ,
                                                 "WOB" = studenten_gediplomeerden[studenten_gediplomeerden$ondCode == "WO" & studenten_gediplomeerden$diploma == "Bachelor",],
                                                 "WOM"= studenten_gediplomeerden[studenten_gediplomeerden$ondCode == "WO" & studenten_gediplomeerden$diploma == "Wo-master",],
                                                 "HBOWO" = aggregate(HWSet$aantal, by=list(iscedNaam=HWSet$iscedCode.iscedNaam, jaartal=HWSet$jaartal), FUN=sum)
    )
    
    #namen kolomtitels van de nieuwe gevormde data aanpassen
    if(input$StudentenGediplomeerden_StudieNiveau == "HBOWO"){
      colnames(StudentenGediplomeerden_StudieSub)<-c("iscedCode.iscedNaam","jaartal","aantal")
    }
    
    #data aanpassen nav keuze gebruiker: studie(s)
    StudentenGediplomeerden_StudieSub <- StudentenGediplomeerden_StudieSub[StudentenGediplomeerden_StudieSub$iscedCode.iscedNaam %in% input$StudentenGediplomeerden_SelectStudyImp,]
    StudentenGediplomeerden_forecastSub <- createForecastSub(StudentenGediplomeerden_StudieSub, "aantal", "iscedCode.iscedNaam", 1995, 2013,"")
    
    PlotTitle <- "Aantal gediplomeerde studenten \nper jaar verdeeld per studie"
    
    if (input$StudentenGediplomeerden_StudieNiveau == "HBOWO"){
      data <- HWSet
    } else {
      data <- studenten_gediplomeerden
    }
    
    SGForecastBaseplot <- ggplot(StudentenGediplomeerden_forecastSub, aes(x=jaartal)) +
      xlab("Jaar") + 
      ylab("Aantal gediplomeerden") +
      ggtitle("Aantal gediplomeerden per studiesector") +
      geom_line(linetype="dashed", size=1,
                aes(y=fitted, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam))+
      geom_line(aes(y=aantal, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam))+
      geom_point(aes(y=aantal, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam))+
      scale_color_manual(values=GetColors(StudentenGediplomeerden_StudieSub$iscedCode.iscedNaam), name = "Studiesector")
    
    if (input$StudentenGediplomeerden_Totaal == TRUE ){
      #totaallijn
      totaalaantal <- TotaalAantal(data = data,
                                   studieNiveauInput = input$StudentenGediplomeerden_StudieNiveau, 
                                   filterParams= c("ondCode",'jaartal',"diploma"))
      forecastTotaal         <- createForecastSub(totaalaantal, "aantal", "singleColumn", 1995, 2013, "")
      forecastTotaal$soort   = "Totaal gediplomeerden" 
      
      SGForecastBaseplot <- SGForecastBaseplot +
        geom_line(data=forecastTotaal, aes(y=aantal, 
                                           group=soort,
                                           color=soort), color = "black") + 
        geom_point(data=forecastTotaal, aes(y=aantal, 
                                            group=soort,
                                            color=soort), color = "black") +
        geom_line(data=forecastTotaal, linetype="dashed", size=1,
                  aes(y=fitted, group=soort, color=soort), color = "black") + 
        geom_ribbon(data=forecastTotaal, aes(ymin=lo80, ymax=hi80, x=jaartal, group=soort), fill="red", alpha=.25) +
        geom_ribbon(data=forecastTotaal, aes(ymin=lo95, ymax=hi95, x=jaartal, group=soort), fill="darkred", alpha=.25)
    }
    
    if (input$StudentenGediplomeerden_TotaalSelect == TRUE && length(input$StudentenGediplomeerden_SelectStudyImp) != 0){
      #alleen select
      totaalaantalselect <- TotaalAantalSelect(data = data,
                                               selectInput = input$StudentenGediplomeerden_SelectStudyImp, 
                                               studieNiveauInput = input$StudentenGediplomeerden_StudieNiveau, 
                                               filterParams= c("ondCode",'jaartal',"diploma"))
      forecastTotaalselect       <- createForecastSub(totaalaantalselect, "aantal", "singleColumn", 1995, 2013, "")
      forecastTotaalselect$soort = "Totaal geselecteerde gediplomeerden"
      
      SGForecastBaseplot <- SGForecastBaseplot +
        geom_line(data=forecastTotaalselect, aes(y=aantal, group=soort, color=soort), 
                  color = "gray48") + 
        geom_point(data=forecastTotaalselect, aes(y=aantal, group=soort, color=soort),
                   color = "gray48") +
        geom_line(data=forecastTotaalselect, linetype="dashed", size=1,
                  aes(y=fitted, group=soort, color=soort), 
                  color = "gray48") +
        geom_ribbon(data=forecastTotaalselect, aes(ymin=lo80, ymax=hi80, x=jaartal, group=soort), fill="blue", alpha=.25) +
        geom_ribbon(data=forecastTotaalselect, aes(ymin=lo95, ymax=hi95, x=jaartal, group=soort), fill="darkblue", alpha=.25)
    }
    
    #Render de plot
    SGForecastBaseplot
  })
  
  observe({
    trueFalse = length(input$StudentenGediplomeerden_SelectStudyImp) == length(unique(studenten_gediplomeerden$iscedCode.iscedNaam))
    
    updateCheckboxInput(session, "StudentenGediplomeerden_AlleStudies", value = trueFalse)
  })
  
  observeEvent(input$StudentenGediplomeerden_AlleStudies, {
    trueFalse = length(input$StudentenGediplomeerden_SelectStudyImp) == length(unique(studenten_gediplomeerden$iscedCode.iscedNaam))
    if(input$StudentenGediplomeerden_AlleStudies == T && !trueFalse){
      updateSelectInput(session, "StudentenGediplomeerden_SelectStudyImp",
                        selected = studenten_gediplomeerden$iscedCode.iscedNaam
      )
    }
  })
}  