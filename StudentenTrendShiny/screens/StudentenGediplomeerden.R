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
                              choices = list(Studiesectoren = unique(studenten_gediplomeerden$iscedCode.iscedNaam), "Selecteer een studiesector" = ""),
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
  reac <- reactiveValues(redraw = TRUE, selections = isolate(input$StudentenGediplomeerden_SelectStudyImp))
  
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
      colnames(StudentenGediplomeerden_StudieSub)<-c("soort","jaartal","aantal")
    } else {
      colnames(StudentenGediplomeerden_StudieSub)[colnames(StudentenGediplomeerden_StudieSub)=="iscedCode.iscedNaam"] <- "soort"
    }
    
    StudentenGediplomeerden_StudieSub <- StudentenGediplomeerden_StudieSub[StudentenGediplomeerden_StudieSub$soort %in% reac$selections,]
    PlotTitle <- "Aantal gediplomeerde studenten per jaar verdeeld per studie"
    
    if (input$StudentenGediplomeerden_StudieNiveau == "HBOWO"){
      data <- HWSet
    } else {
      data <- studenten_gediplomeerden
    }
    
    #Alleen select
    if (input$StudentenGediplomeerden_TotaalSelect == TRUE && length(reac$selections) != 0){
      StudentenGediplomeerden_StudieSub <- TotaalAantalSelect(data = StudentenGediplomeerden_StudieSub,
                                                              studieNiveauInput = input$StudentenGediplomeerden_StudieNiveau,
                                                              filterParams= c("ondCode",'jaartal',"diploma"))
    }
    
    #Totaal lijn toevoegen
    if (input$StudentenGediplomeerden_Totaal == TRUE ){
      #totaallijn
      StudentenGediplomeerden_StudieSub <- TotaalAantal(data = data,
                                                        subSet = StudentenGediplomeerden_StudieSub,
                                                        studieNiveauInput = input$StudentenGediplomeerden_StudieNiveau, 
                                                        filterParams = c("ondCode",'jaartal',"diploma"))
    }
    
    StudentenGediplomeerden_StudieSub <<- StudentenGediplomeerden_StudieSub
    
    SGLineBaseplot <- ggplot(StudentenGediplomeerden_StudieSub, aes(x=jaartal)) + 
      xlab("Jaar") +  
      ylab("Aantal studenten") + 
      ggtitle(PlotTitle) +
      theme(legend.position="none") +
      geom_line(data=StudentenGediplomeerden_StudieSub, size = -1,
                aes(y=aantal, group=soort, color=soort)) + 
      geom_point(data=StudentenGediplomeerden_StudieSub,
                 aes(y=aantal, group=soort, color=soort)) +
      scale_color_manual(values=GetColors(StudentenGediplomeerden_StudieSub$soort), labels=unique(StudentenGediplomeerden_StudieSub$soort))
    
    #Render de plot
    if( length(reac$selections) != 0 || input$StudentenGediplomeerden_Totaal == TRUE) {
      PrintGGPlotly(SGLineBaseplot)
    } else {
      return(SGLineBaseplot)
    }
  })
  
  ######################
  ## NORMALE BAR PLOT ##
  ######################
  output$DiploBarPlot <- renderPlot({
    HWBarSet <- studenten_gediplomeerden[(studenten_gediplomeerden$ondCode == "HBO" & studenten_gediplomeerden$diploma == "Bachelor") | (studenten_gediplomeerden$ondCode == "WO" & studenten_gediplomeerden$diploma == "Wo-master") ,] 
    
    #data aanpassen nav keuzes gebruiker
    StudentenGediplomeerden_StudieBarSub <- switch (input$StudentenGediplomeerden_StudieNiveau,
                                                    "HBO" = studenten_gediplomeerden[studenten_gediplomeerden$ondCode == "HBO" & studenten_gediplomeerden$diploma == "Bachelor",] ,
                                                    "WOB" = studenten_gediplomeerden[studenten_gediplomeerden$ondCode == "WO" & studenten_gediplomeerden$diploma == "Bachelor",],
                                                    "WOM"= studenten_gediplomeerden[studenten_gediplomeerden$ondCode == "WO" & studenten_gediplomeerden$diploma == "Wo-master",],
                                                    "HBOWO" = aggregate(HWBarSet$aantal, by=list(iscedNaam=HWBarSet$iscedCode.iscedNaam, jaartal=HWBarSet$jaartal), FUN=sum)
    )
    
    #namen kolomtitels van de nieuwe gevormde data aanpassen
    if(input$StudentenGediplomeerden_StudieNiveau == "HBOWO"){
      colnames(StudentenGediplomeerden_StudieBarSub)<-c("soort","jaartal","aantal")
    }
    
    StudentenGediplomeerden_StudieBarSub <- StudentenGediplomeerden_StudieBarSub[StudentenGediplomeerden_StudieBarSub$iscedCode.iscedNaam %in% reac$selections,]
    PlotTitle <- "Aantal gediplomeerde studenten per jaar verdeeld per studie"
    
    if (input$StudentenGediplomeerden_StudieNiveau == "HBOWO"){
      data <- HWBarSet
    } else {
      data <- studenten_gediplomeerden
    }
    
    #scale_color_manual options
    scmOptionsList <- InitGGLegend()
    
    if (input$StudentenGediplomeerden_TotaalSelect == TRUE && length(reac$selections) != 0){
      StudentenGediplomeerden_StudieBarSub <- TotaalAantalSelect(data = StudentenGediplomeerden_StudieBarSub,
                                                                 studieNiveauInput = input$StudentenGediplomeerden_StudieNiveau,
                                                                 filterParams= c("ondCode",'jaartal',"diploma"))
    }
    
    if (input$StudentenGediplomeerden_Totaal == TRUE ){
      StudentenGediplomeerden_StudieBarSub <- TotaalAantal(data = data, 
                                                           subSet = StudentenGediplomeerden_StudieBarSub,
                                                           studieNiveauInput = input$StudentenGediplomeerden_StudieNiveau, 
                                                           filterParams= c("ondCode",'jaartal',"diploma"))
    }
      
    SGBarBaseplot <- ggplot(StudentenGediplomeerden_StudieBarSub, aes(x=jaartal)) + 
      xlab("Jaar") +  
      ylab("Aantal studenten") + 
      ggtitle(PlotTitle) +
      geom_bar(stat = "identity", aes(y=aantal, fill=iscedCode.iscedNaam)) + 
      scale_fill_manual(values=GetColors(StudentenGediplomeerden_StudieBarSub$iscedCode.iscedNaam), name= "Studiesector")
    
    AddTotaalLines(plot=SGBarBaseplot, data=StudentenGediplomeerden_StudieBarSub)
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
      colnames(StudentenGediplomeerden_StudieSub)<-c("soort","jaartal","aantal")
    } else {
      colnames(StudentenGediplomeerden_StudieSub)[colnames(StudentenGediplomeerden_StudieSub)=="iscedCode.iscedNaam"] <- "soort"
    }
    
    #data aanpassen nav keuze gebruiker: studie(s)
    StudentenGediplomeerden_StudieSub   <- StudentenGediplomeerden_StudieSub[StudentenGediplomeerden_StudieSub$soort %in% reac$selections,]
    PlotTitle <- "Aantal gediplomeerde studenten per jaar verdeeld per studie"
    
    StudentenGediplomeerden_StudieSub <<- StudentenGediplomeerden_StudieSub
    
    if (input$StudentenGediplomeerden_StudieNiveau == "HBOWO"){
      data <- HWSet
    } else {
      data <- studenten_gediplomeerden
    }
    
    if (input$StudentenGediplomeerden_TotaalSelect == TRUE && length(reac$selections) != 0){
      StudentenGediplomeerden_StudieSub <- TotaalAantalSelect(data = StudentenGediplomeerden_StudieSub,
                                                              studieNiveauInput = input$StudentenGediplomeerden_StudieNiveau, 
                                                              filterParams= c("ondCode",'jaartal',"diploma"))
    }
    
    #Totaallijn
    if (input$StudentenGediplomeerden_Totaal == TRUE ){
      StudentenGediplomeerden_StudieSub <- TotaalAantal(data = data,
                                                        subSet = StudentenGediplomeerden_StudieSub,
                                                        studieNiveauInput = input$StudentenGediplomeerden_StudieNiveau, 
                                                        filterParams= c("ondCode",'jaartal',"diploma"))
    }
    
    StudentenGediplomeerden_StudieSub <<- StudentenGediplomeerden_StudieSub
    
    StudentenGediplomeerden_forecastSub <- createForecastSub(StudentenGediplomeerden_StudieSub, "aantal", "soort", 1995, 2013,"")
    
    SGForecastBaseplot <- ggplot(StudentenGediplomeerden_forecastSub, aes(x=jaartal)) +
      ggtitle(PlotTitle) +
      xlab("Jaar") + 
      ylab("Aantal gediplomeerden")
    
    StudentenGediplomeerden_forecastSub <<- StudentenGediplomeerden_forecastSub
  
    AddTotaalLines(plot=SGForecastBaseplot, data=StudentenGediplomeerden_forecastSub)  
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
  
  observe({
    input$StudentenGediplomeerden_SelectStudyImp
    
    reac$redraw <- FALSE
  })
  
  observe({
    invalidateLater(500, session)
    input$StudentenGediplomeerden_SelectStudyImp
    input$redraw
    if (isolate(reac$redraw)) {
      reac$selections <- input$StudentenGediplomeerden_SelectStudyImp
    } else {
      isolate(reac$redraw <- TRUE)
    }
  })
}  