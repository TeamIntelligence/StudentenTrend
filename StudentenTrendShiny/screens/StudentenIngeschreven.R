StudentenIngeschrevenUI <- function(PageName){
  return(  
    tabItem(tabName = PageName,
      # Page title
      fluidRow(
        box(width = 12, collapsible = T, title="Ingeschreven studenten",
            p("Op deze pagina vindt u het aantal ingeschreven studenten per studiesector over de periode 1990 tot en met 2014. U kunt zelf kiezen welke studiesectoren u wilt weergeven. Daarnaast kunt u ook zelf een keuze maken tussen HBO en WO. Verder kan u ook een totaallijn weergeven van alle studies of een totaallijn van de studies die u geselecteerd hebt."),
            p("De grafiek biedt inzicht hoeveel studenten elk jaar ingeschreven zijn voor een studie binnen een bepaalde studiesector. Er kan vervolgens uit opgemaakt worden hoe een studie binnen een bepaalde studiesector zich ontwikkeld qua grootte.")
            ),
        box(width=4, height = 170, 
          selectInput("StudentenIngeschreven_SelectStudyImp",
                      "Selecteer een of meerdere studiesectoren om weer te geven:",
                      choices = studenten_ingeschrevenen$iscedCode.iscedNaam,
                      multiple = TRUE,
                      selectize = TRUE
          ),
          checkboxInput("StudentenIngeschreven_AlleStudies",
                        "Selecteer alle studies"
          )
        ),
        box(width = 4, height = 170,
          radioButtons("StudentenIngeschreven_StudieNiveau",
                       "Studie Niveau", 
                       choices = list("HBO" = "HBO", 
                                      "WO"  = "WO",
                                      "HBO en WO" = "HBOWO")
          )
        ),
        box(width = 4, height = 170,
          checkboxInput("StudentenIngeschreven_Totaalselect",
                        "Totaal lijn weergeven van de geselecteerde studies"
          ),
          checkboxInput("StudentenIngeschreven_Totaal",
                      "Totaal lijn weergeven"
          )
          
        ),
        
        tabBox(width=12, height=550, 
               tabPanel("Huidige data",
                        box(width=5, plotOutput("aantalIngeschrevenPlot", height=450)),
                        box(width=7, plotOutput("aantalIngeschrevenBarPlot", height=450))
               ),
               tabPanel("Voorspelling",
                        box(width=12,plotOutput("aantalIngeschrevenVoorspellingPlot", height = 450))
               )
        )
      )
    )
  )
}

StudentenIngeschrevenServer <- function(input, output, session){
  
  #######################
  ## NORMALE LINE PLOT ##
  #######################
  output$aantalIngeschrevenPlot <- renderPlot({
    #totaallijn
    totaalaantal <- TotaalAantal(data = studenten_ingeschrevenen,
                                 studieNiveauInput = input$StudentenIngeschreven_StudieNiveau, 
                                 filterParams= c("ondCode",'jaartal'))
    
    #data aanpassen nav keuzes gebruiker: studieniveau
    siSub <- switch (input$StudentenIngeschreven_StudieNiveau,
                     "HBO" = studenten_ingeschrevenen[studenten_ingeschrevenen$ondCode == "HBO",],
                     "WO" = studenten_ingeschrevenen[studenten_ingeschrevenen$ondCode == "WO",],
                     "HBOWO" = aggregate(studenten_ingeschrevenen$aantal, by=list(iscedNaam=studenten_ingeschrevenen$iscedCode.iscedNaam,jaartal=studenten_ingeschrevenen$jaartal), FUN=sum)
    )
    
    #namen kolomtitels van de nieuwe gevormde data aanpassen
    if(input$StudentenIngeschreven_StudieNiveau == "HBOWO"){
      colnames(siSub)<-c("iscedCode.iscedNaam","jaartal","aantal")
    }
    
    #data aanpassen nav keuze gebruiker: studie(s)
    siSub <- siSub[siSub$iscedCode.iscedNaam %in% input$StudentenIngeschreven_SelectStudyImp,]
    
    #plotten en titel laten afhangen
    plotTitle <- paste("Aantal ingeschreven bachelor studenten \nper jaar verdeeld per studie")
    
    #Basis plot maken
    SILineBaseplot <- ggplot(siSub, aes(x=jaartal)) + 
      xlab("Jaar") +  
      ylab("Aantal studenten") + 
      ggtitle(plotTitle) +
      geom_line(data=siSub, 
                aes(y=aantal, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam)) + 
      geom_point(data=siSub,
                 aes(y=aantal, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam)) +
      scale_color_manual(values=GetColors(siSub$iscedCode.iscedNaam)) +
      theme(legend.position="none")
    
    #Totaal lijn toevoegen
    if (input$StudentenIngeschreven_Totaal == TRUE ){
      SILineBaseplot <- SILineBaseplot +
        geom_line(data=totaalaantal, 
                  aes(y=aantal, group=ondCode, color=ondCode), 
                  color = "black") + 
        geom_point(data=totaalaantal, 
                   aes(y=aantal, group=ondCode, color=ondCode), 
                   color = "black")
    }
    
    #Totaal geselecteerd lijn toevoegen
    if (input$StudentenIngeschreven_Totaalselect == TRUE && length(input$StudentenIngeschreven_SelectStudyImp) != 0){
      
      totaalaantalselect <- TotaalAantalSelect(data = studenten_ingeschrevenen,
                                               selectInput = input$StudentenIngeschreven_SelectStudyImp, 
                                               studieNiveauInput = input$StudentenIngeschreven_StudieNiveau, 
                                               filterParams= c("ondCode",'jaartal'))
      
      SILineBaseplot <- SILineBaseplot +
        geom_line(data=totaalaantalselect, 
                  aes(y=aantal, group=ondCode, olor=ondCode),
                  color = "gray48") + 
        geom_point(data=totaalaantalselect, 
                   aes(y=aantal, group=ondCode, color=ondCode),
                   color = "gray48")
    }
    
    #Render plot
    SILineBaseplot
  })
  
  ######################
  ## NORMALE BAR PLOT ##
  ######################
  output$aantalIngeschrevenBarPlot <- renderPlot({
    #totaallijn
    totaalaantal <- TotaalAantal(data = studenten_ingeschrevenen,
                                 studieNiveauInput = input$StudentenIngeschreven_StudieNiveau, 
                                 filterParams= c("ondCode",'jaartal'))
    
    #data aanpassen nav keuzes gebruiker: studieniveau
    siBarSub <- switch (input$StudentenIngeschreven_StudieNiveau,
                        "HBO" = studenten_ingeschrevenen[studenten_ingeschrevenen$ondCode == "HBO",],
                        "WO" = studenten_ingeschrevenen[studenten_ingeschrevenen$ondCode == "WO",],
                        "HBOWO" = aggregate(studenten_ingeschrevenen$aantal, by=list(iscedNaam=studenten_ingeschrevenen$iscedCode.iscedNaam,jaartal=studenten_ingeschrevenen$jaartal), FUN=sum)
    )
    
    #namen kolomtitels van de nieuwe gevormde data aanpassen
    if(input$StudentenIngeschreven_StudieNiveau == "HBOWO"){
      colnames(siBarSub)<-c("iscedCode.iscedNaam","jaartal","aantal")
    } 
    
    #data aanpassen nav keuze gebruiker: studie(s)
    siBarSub <- siBarSub[siBarSub$iscedCode.iscedNaam %in% input$StudentenIngeschreven_SelectStudyImp,]
    
    #plotten en titel laten afhangen
    plotTitle <- paste("Aantal ingeschreven bachelor studenten \nper jaar verdeeld per studie")
    
    #scale_color_manual options
    scmOptionsList.names <- c("values", "breaks", "labels")
    scmOptionsList <- setNames(vector("list", length(scmOptionsList.names )), scmOptionsList.names )
    
    SIBarBaseplot <- ggplot(siBarSub, aes(x=jaartal)) + 
      xlab("Jaar") +  
      ylab("Aantal studenten") + 
      ggtitle(plotTitle) +
      geom_bar(data = siBarSub, stat = "identity",
               aes(y=aantal, fill=iscedCode.iscedNaam))+
      scale_fill_manual(values=GetColors(siBarSub$iscedCode.iscedNaam),name="Studierichting")
      
    if (input$StudentenIngeschreven_Totaal == TRUE ){
      
      SIBarBaseplot <- SIBarBaseplot +
        geom_line(data=totaalaantal, 
                  aes(y=aantal, group=ondCode, color= "black")) + 
        geom_point(data=totaalaantal, 
                   aes(y=aantal, group=ondCode, color="black"))+
        labs(color = "Totaallijn")
      
      scmOptionsList$values <- c(scmOptionsList$values, "black")
      scmOptionsList$breaks <- c(scmOptionsList$breaks, "black")
      scmOptionsList$labels <- c(scmOptionsList$labels, "Totaallijn")
    } 
    
    if (input$StudentenIngeschreven_Totaalselect == TRUE && length(input$StudentenIngeschreven_SelectStudyImp) != 0){
      totaalaantalselect <- TotaalAantalSelect(data = studenten_ingeschrevenen,
                                               selectInput = input$StudentenIngeschreven_SelectStudyImp, 
                                               studieNiveauInput = input$StudentenIngeschreven_StudieNiveau, 
                                               filterParams= c("ondCode",'jaartal'))
      
      SIBarBaseplot <- SIBarBaseplot +
        geom_line(data=totaalaantalselect, 
                  aes(y=aantal, group=ondCode, color= "gray48")) + 
        geom_point(data=totaalaantalselect, 
                   aes(y=aantal, group=ondCode, color="gray48")) +
        labs(color = "Totaallijn")
      
      scmOptionsList$values <- c(scmOptionsList$values, "gray48")
      scmOptionsList$breaks <- c(scmOptionsList$breaks, "gray48")
      scmOptionsList$labels <- c(scmOptionsList$labels, "Totaallijn geselecteerde")
    } 
    
    #Renderplot
    SIBarBaseplot +
      scale_color_manual(values=scmOptionsList$values,breaks=scmOptionsList$breaks, labels=scmOptionsList$labels)
  })

  #########################
  ## VOORSPELLINGEN PLOT ##
  #########################
  output$aantalIngeschrevenVoorspellingPlot <- renderPlot({
    
    #data aanpassen nav keuzes gebruiker: studieniveau
    siSub <- switch (input$StudentenIngeschreven_StudieNiveau,
                     "HBO" = studenten_ingeschrevenen[studenten_ingeschrevenen$ondCode == "HBO",],
                     "WO" = studenten_ingeschrevenen[studenten_ingeschrevenen$ondCode == "WO",],
                     "HBOWO" = aggregate(studenten_ingeschrevenen$aantal, by=list(iscedNaam=studenten_ingeschrevenen$iscedCode.iscedNaam,jaartal=studenten_ingeschrevenen$jaartal), FUN=sum)
    )
    
    #namen kolomtitels van de nieuwe gevormde data aanpassen
    if(input$StudentenIngeschreven_StudieNiveau == "HBOWO"){
      colnames(siSub)<-c("iscedCode.iscedNaam","jaartal","aantal")
    }
    
    #data aanpassen nav keuze gebruiker: studie(s) en forecast maken
    siSub<-siSub[siSub$iscedCode.iscedNaam %in% input$StudentenIngeschreven_SelectStudyImp,]
#     if (is.null(input$StudentenIngeschreven_SelectStudyImp)==TRUE){
#       StudentenIngeschreven_forecastSub<<-siSub
#       }
#     else{
      StudentenIngeschreven_forecastSub <- createForecastSub(siSub, "aantal", "iscedCode.iscedNaam", 1990, 2014,"")
#    }

    #totaallijn
    totaalaantal <- TotaalAantal(data = studenten_ingeschrevenen,
                                 studieNiveauInput = input$StudentenIngeschreven_StudieNiveau, 
                                 filterParams= c("ondCode",'jaartal'))
    forecastTotaal         <- createForecastSub(totaalaantal, "aantal", "singleColumn", 1990, 2014, "")
    forecastTotaal$soort   = "Totaal ingeschrevenen" 
    
    #scale_color_manual options
    scmOptionsList.names <- c("values", "breaks", "labels")
    scmOptionsList <- setNames(vector("list", length(scmOptionsList.names )), scmOptionsList.names)
    
    scmOptionsList$values <- NULL
    scmOptionsList$breaks <- NULL
    scmOptionsList$labels <- NULL
    
    SIForecastBaseplot <- ggplot(StudentenIngeschreven_forecastSub, aes(x=jaartal)) +
      xlab("Jaar") + 
      ylab("Aantal ingeschreven studenten") +
      ggtitle("Aantal ingeschreven studenten per studiesector") +
       geom_line(linetype="dashed", size=1,
                 aes(y=fitted, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam))+
      geom_line(aes(y=aantal, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam))+
      geom_point(aes(y=aantal, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam))

     scmOptionsList$values <- c(scmOptionsList$values, GetColors(siSub$iscedCode.iscedNaam))
     scmOptionsList$breaks <- c(scmOptionsList$breaks, GetColors(siSub$iscedCode.iscedNaam))
     scmOptionsList$labels <- c(scmOptionsList$labels, unique(siSub$iscedCode.iscedNaam))
#    
     
     if (input$StudentenIngeschreven_Totaalselect == TRUE ){
       
       totaalaantalselect <- TotaalAantalSelect(data = studenten_ingeschrevenen,
                                                selectInput = input$StudentenIngeschreven_SelectStudyImp, 
                                                studieNiveauInput = input$StudentenIngeschreven_StudieNiveau, 
                                                filterParams= c("ondCode",'jaartal'))
       
       forecastTotaalselect         <- createForecastSub(totaalaantalselect, "aantal", "singleColumn", 1990, 2014, "")
       forecastTotaalselect$soort   = "Totaal geselecteerde ingeschreven studenten"
       
       SIForecastBaseplot <- SIForecastBaseplot +
         geom_line(data=forecastTotaalselect, aes(y=aantal, 
                                                  group=soort,
                                                  color="gray48"),color="gray48") + 
         geom_point(data=forecastTotaalselect, aes(y=aantal, 
                                                   group=soort,
                                                   color="gray48"),color="gray48") +
         geom_line(data=forecastTotaalselect, linetype="dashed", size=1,
                   aes(y=fitted, group=soort, color="gray48")) +
         
         geom_ribbon(data=forecastTotaalselect, aes(ymin=lo80, ymax=hi80, x=jaartal, group=soort), fill="blue", alpha=.25) +
         geom_ribbon(data=forecastTotaalselect, aes(ymin=lo95, ymax=hi95, x=jaartal, group=soort), fill="darkblue", alpha=.25)#+
       #labs(color = "Totaallijn")
       
       scmOptionsList$values <- c("gray48",scmOptionsList$values)
       scmOptionsList$breaks <- c("gray48", scmOptionsList$breaks)
       scmOptionsList$labels <- c("Totaallijn geselecteerde",scmOptionsList$labels)
       
     }
     
    #alleen totaal
    if (input$StudentenIngeschreven_Totaal == TRUE ){
      
      SIForecastBaseplot <- SIForecastBaseplot +
        geom_line(data=forecastTotaal, aes(y=aantal, 
                                           group=soort,
                                           color="black"), color="black") + 
        geom_point(data=forecastTotaal, aes(y=aantal, 
                                            group=soort,
                                            color="black"), color="black")+
        geom_line(data=forecastTotaal, linetype="dashed", size=1,
                  aes(y=fitted, group=soort, color="black")) + 
        geom_ribbon(data=forecastTotaal, aes(ymin=lo80, ymax=hi80, x=jaartal, group=soort), fill="red", alpha=.25) +
        geom_ribbon(data=forecastTotaal, aes(ymin=lo95, ymax=hi95, x=jaartal, group=soort), fill="darkred", alpha=.25)
        #labs(color = "Totaallijn")
      
      scmOptionsList$values <- c("black",scmOptionsList$values)
      scmOptionsList$breaks <- c("black",scmOptionsList$breaks)
      scmOptionsList$labels <- c("Totaallijn",scmOptionsList$labels)
      
      
      
    }

    #Render de plot
    SIForecastBaseplot+
      scale_color_manual(values=scmOptionsList$values, labels=scmOptionsList$labels, name="Studierichting")
    })
  
  observe({
    trueFalse = length(input$StudentenIngeschreven_SelectStudyImp) == length(unique(studenten_ingeschrevenen$iscedCode.iscedNaam))
    
    updateCheckboxInput(session, "StudentenIngeschreven_AlleStudies", value = trueFalse)
    
  })
  observeEvent(input$StudentenIngeschreven_AlleStudies, {
    trueFalse = length(input$StudentenIngeschreven_SelectStudyImp) == length(unique(studenten_ingeschrevenen$iscedCode.iscedNaam))
    if(input$StudentenIngeschreven_AlleStudies == T && !trueFalse){
      updateSelectInput(session, "StudentenIngeschreven_SelectStudyImp",
                        selected = studenten_ingeschrevenen$iscedCode.iscedNaam
      )
    }
  })
}   
  