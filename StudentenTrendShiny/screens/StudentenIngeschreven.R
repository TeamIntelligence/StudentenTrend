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
        
      )
      
      # Show a plot of the generated distribution
      ,box(width=5, height = 470, plotOutput("aantalIngeschrevenPlot", height=450))
      ,box(width=7, height = 470, plotOutput("aantalIngeschrevenBarPlot", height=450))
      )
    )
  )
}



StudentenIngeschrevenServer <- function(input, output, session){
  
  output$aantalIngeschrevenPlot <- renderPlot({
    
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
    
    
    if (input$StudentenIngeschreven_Totaal == TRUE & input$StudentenIngeschreven_Totaalselect == TRUE ){ 
      
      ##allebei de lijnen
      
      ##select lijn
      ##keuze maken welke studies
      totaalaantalselect <- studenten_ingeschrevenen[studenten_ingeschrevenen$iscedCode.iscedNaam %in% input$StudentenIngeschreven_SelectStudyImp,]
      
      #Totaal berekenen
      totaalaantalselect <- aggregate(totaalaantalselect$aantal, by=list(ondCode=totaalaantalselect$ondCode,jaartal=totaalaantalselect$jaartal), FUN=sum)
      colnames(totaalaantalselect)<-c("ondCode","jaartal", "aantal")
      
      
      #keuze maken welk studie niveau
      
      totaalaantalselect <- switch (input$StudentenIngeschreven_StudieNiveau,
                                    "HBO" = totaalaantalselect[totaalaantalselect$ondCode == "HBO",],
                                    "WO" = totaalaantalselect[totaalaantalselect$ondCode == "WO",],
                                    "HBOWO" = aggregate(totaalaantalselect$aantal, by=list(jaartal=totaalaantalselect$jaartal), FUN=sum)
      )        
      if (input$StudentenIngeschreven_StudieNiveau == "HBOWO"){
        colnames(totaalaantalselect)<-c("jaartal","aantal")
        totaalaantalselect$ondCode = "Totaal geselecteerde HBO en WO studies"
      }
      if (input$StudentenIngeschreven_StudieNiveau == "HBO"){
        totaalaantalselect$ondCode = "Totaal geselecteerde HBO studies"
      }
      if (input$StudentenIngeschreven_StudieNiveau == "WO"){
        totaalaantalselect$ondCode = "Totaal geselecteerde WO studies"
      }
      
      #totaallijn
      #Totaal berekenen
      totaalaantal <- aggregate(studenten_ingeschrevenen$aantal, by=list(ondCode=studenten_ingeschrevenen$ondCode,jaartal=studenten_ingeschrevenen$jaartal), FUN=sum)
      colnames(totaalaantal)<-c("ondCode","jaartal","aantal")
      
      
      #keuze maken welk studie niveau
      totaalaantal <- switch (input$StudentenIngeschreven_StudieNiveau,
                              "HBO" = totaalaantal[totaalaantal$ondCode == "HBO",],
                              "WO" = totaalaantal[totaalaantal$ondCode == "WO",],
                              "HBOWO" = aggregate(totaalaantal$aantal, by=list(jaartal=totaalaantal$jaartal), FUN=sum)
      )        
      if (input$StudentenIngeschreven_StudieNiveau == "HBOWO"){
        colnames(totaalaantal)<-c("jaartal","aantal")
        totaalaantal$ondCode = "Totaal HBO en WO"
      }
      if (input$StudentenIngeschreven_StudieNiveau == "HBO"){
        totaalaantal$ondCode = "Totaal HBO"
      }
      if (input$StudentenIngeschreven_StudieNiveau == "WO"){
        totaalaantal$ondCode = "Totaal WO"
      }

      
      #plotten
      ggplot(totaalaantal,   
             aes(x=jaartal)) + 
        xlab("Jaar") +  
        ylab("Aantal studenten") + 
        ggtitle(plotTitle) +
        geom_line(data=siSub, aes(y=aantal,     #lijn studies
                                  group=iscedCode.iscedNaam,
                                  color=iscedCode.iscedNaam)) + 
        geom_point(data=siSub,aes(y=aantal, 
                                  group=iscedCode.iscedNaam,
                                  color=iscedCode.iscedNaam)) +
        geom_line(data=totaalaantal, aes(y=aantal,  #totaal lijn
                                         group=ondCode,
                                         color= ondCode), color = "black") + 
        geom_point(data=totaalaantal, aes(y=aantal, 
                                          group=ondCode,
                                          color=ondCode), color = "black") + 
        geom_line(data=totaalaantalselect, aes(y=aantal,  #totaal select lijn
                                               group=ondCode,
                                               color=ondCode), color = "gray48") +  
        geom_point(data=totaalaantalselect, aes(y=aantal, 
                                                group=ondCode,
                                                color=ondCode), color = "gray48") + 
        labs(color = "Studierichting")+ 
        theme(legend.position="none")
      
      
    }
    else if (input$StudentenIngeschreven_Totaalselect == TRUE ){
      #alleen select
      
      ##keuze maken welke studies
      totaalaantalselect <- studenten_ingeschrevenen[studenten_ingeschrevenen$iscedCode.iscedNaam %in% input$StudentenIngeschreven_SelectStudyImp,]
    
      #Totaal berekenen
      totaalaantalselect <- aggregate(totaalaantalselect$aantal, by=list(ondCode=totaalaantalselect$ondCode,jaartal=totaalaantalselect$jaartal), FUN=sum)
      colnames(totaalaantalselect)<-c("ondCode","jaartal", "aantal")
      
      
      #keuze maken welk studie niveau
      
      totaalaantalselect <- switch (input$StudentenIngeschreven_StudieNiveau,
                                    "HBO" = totaalaantalselect[totaalaantalselect$ondCode == "HBO",],
                                    "WO" = totaalaantalselect[totaalaantalselect$ondCode == "WO",],
                                    "HBOWO" = aggregate(totaalaantalselect$aantal, by=list(jaartal=totaalaantalselect$jaartal), FUN=sum)
      )        
      if (input$StudentenIngeschreven_StudieNiveau == "HBOWO"){
        colnames(totaalaantalselect)<-c("jaartal","aantal")
        totaalaantalselect$ondCode = "Totaal geselecteerde HBO en WO studies"
      }
      if (input$StudentenIngeschreven_StudieNiveau == "HBO"){
        totaalaantalselect$ondCode = "Totaal geselecteerde HBO studies"
      }
      if (input$StudentenIngeschreven_StudieNiveau == "WO"){
        totaalaantalselect$ondCode = "Totaal geselecteerde WO studies"
      }
      
      
      ggplot(totaalaantalselect,   
             aes(x=jaartal)) + 
        xlab("Jaar") +  
        ylab("Aantal studenten") + 
        ggtitle(plotTitle) +
        geom_line(data=siSub, aes(y=aantal,     #lijn studies
                                  group=iscedCode.iscedNaam,
                                  color=iscedCode.iscedNaam)) + 
        geom_point(data=siSub,aes(y=aantal, 
                                  group=iscedCode.iscedNaam,
                                  color=iscedCode.iscedNaam)) +
        geom_line(data=totaalaantalselect, aes(y=aantal,  #totaal lijn
                                               group=ondCode,
                                               color=ondCode), color = "gray48") + 
        geom_point(data=totaalaantalselect, aes(y=aantal, 
                                                group=ondCode,
                                                color=ondCode), color = "gray48") +
        labs(color = "Studierichting")+ 
        theme(legend.position="none")
      
      
      ##############
    }
    else if (input$StudentenIngeschreven_Totaal == TRUE ){
      #alleen totaal
      #Totaal berekenen
      totaalaantal <- aggregate(studenten_ingeschrevenen$aantal, by=list(ondCode=studenten_ingeschrevenen$ondCode,jaartal=studenten_ingeschrevenen$jaartal), FUN=sum)
      colnames(totaalaantal)<-c("ondCode","jaartal","aantal")
      
      
      #keuze maken welk studie niveau
      totaalaantal <- switch (input$StudentenIngeschreven_StudieNiveau,
                              "HBO" = totaalaantal[totaalaantal$ondCode == "HBO",],
                              "WO" = totaalaantal[totaalaantal$ondCode == "WO",],
                              "HBOWO" = aggregate(totaalaantal$aantal, by=list(jaartal=totaalaantal$jaartal), FUN=sum)
      )        
      if (input$StudentenIngeschreven_StudieNiveau == "HBOWO"){
        colnames(totaalaantal)<-c("jaartal","aantal")
        totaalaantal$ondCode = "Totaal HBO en WO"
      }
      if (input$StudentenIngeschreven_StudieNiveau == "HBO"){
        totaalaantal$ondCode = "Totaal HBO"
      }
      if (input$StudentenIngeschreven_StudieNiveau == "WO"){
        totaalaantal$ondCode = "Totaal WO"
      }
      
      ggplot(totaalaantal,   
             aes(x=jaartal)) + 
        xlab("Jaar") +  
        ylab("Aantal studenten") + 
        ggtitle(plotTitle) +
        geom_line(data=siSub, aes(y=aantal,     #lijn studies
                                  group=iscedCode.iscedNaam,
                                  color=iscedCode.iscedNaam)) + 
        geom_point(data=siSub,aes(y=aantal, 
                                  group=iscedCode.iscedNaam,
                                  color=iscedCode.iscedNaam)) +
        geom_line(data=totaalaantal, aes(y=aantal,  #totaal lijn
                                         group=ondCode,
                                         color=ondCode), color = "black") + 
        geom_point(data=totaalaantal, aes(y=aantal, 
                                          group=ondCode,
                                          color=ondCode), color = "black") +
        labs(color = "Studierichting")+ 
        theme(legend.position="none")
    }
    else{
      #normale enkele plot
      ggplot(siSub, 
             aes(x=jaartal)) + 
        xlab("Jaar") +  
        ylab("Aantal studenten") + 
        ggtitle(plotTitle) +
        geom_line(aes(y=aantal, 
                      group=iscedCode.iscedNaam,
                      color=iscedCode.iscedNaam)) + 
        geom_point(aes(y=aantal, 
                       group=iscedCode.iscedNaam,
                       color=iscedCode.iscedNaam)) +
        labs(color = "Studierichting")+ 
        theme(legend.position="none")
    }
    
    
    
    
  })
  
  
  output$aantalIngeschrevenBarPlot <- renderPlot({
    
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
    
    
    if (input$StudentenIngeschreven_Totaal == TRUE & input$StudentenIngeschreven_Totaalselect == TRUE ){ 
      
      ##allebei de lijnen
      
      ##select lijn
      ##keuze maken welke studies
      totaalaantalselect <- studenten_ingeschrevenen[studenten_ingeschrevenen$iscedCode.iscedNaam %in% input$StudentenIngeschreven_SelectStudyImp,]
      
      #Totaal berekenen
      totaalaantalselect <- aggregate(totaalaantalselect$aantal, by=list(ondCode=totaalaantalselect$ondCode,jaartal=totaalaantalselect$jaartal), FUN=sum)
      colnames(totaalaantalselect)<-c("ondCode","jaartal", "aantal")
      
      
      #keuze maken welk studie niveau
      
      totaalaantalselect <- switch (input$StudentenIngeschreven_StudieNiveau,
                                    "HBO" = totaalaantalselect[totaalaantalselect$ondCode == "HBO",],
                                    "WO" = totaalaantalselect[totaalaantalselect$ondCode == "WO",],
                                    "HBOWO" = aggregate(totaalaantalselect$aantal, by=list(jaartal=totaalaantalselect$jaartal), FUN=sum)
      )        
      if (input$StudentenIngeschreven_StudieNiveau == "HBOWO"){
        colnames(totaalaantalselect)<-c("jaartal","aantal")
        totaalaantalselect$ondCode = "Totaal geselecteerde HBO en WO studies"
      }
      if (input$StudentenIngeschreven_StudieNiveau == "HBO"){
        totaalaantalselect$ondCode = "Totaal geselecteerde HBO studies"
      }
      if (input$StudentenIngeschreven_StudieNiveau == "WO"){
        totaalaantalselect$ondCode = "Totaal geselecteerde WO studies"
      }
      
      #totaallijn
      #Totaal berekenen
      totaalaantal <- aggregate(studenten_ingeschrevenen$aantal, by=list(ondCode=studenten_ingeschrevenen$ondCode,jaartal=studenten_ingeschrevenen$jaartal), FUN=sum)
      colnames(totaalaantal)<-c("ondCode","jaartal","aantal")
      
      
      #keuze maken welk studie niveau
      totaalaantal <- switch (input$StudentenIngeschreven_StudieNiveau,
                              "HBO" = totaalaantal[totaalaantal$ondCode == "HBO",],
                              "WO" = totaalaantal[totaalaantal$ondCode == "WO",],
                              "HBOWO" = aggregate(totaalaantal$aantal, by=list(jaartal=totaalaantal$jaartal), FUN=sum)
      )        
      if (input$StudentenIngeschreven_StudieNiveau == "HBOWO"){
        colnames(totaalaantal)<-c("jaartal","aantal")
        totaalaantal$ondCode = "Totaal HBO en WO"
      }
      if (input$StudentenIngeschreven_StudieNiveau == "HBO"){
        totaalaantal$ondCode = "Totaal HBO"
      }
      if (input$StudentenIngeschreven_StudieNiveau == "WO"){
        totaalaantal$ondCode = "Totaal WO"
      }
      
      
      #plotten
      ggplot(siBarSub, 
             aes(x=jaartal)) + 
        xlab("Jaar") +  
        ylab("Aantal studenten") + 
        ggtitle(plotTitle) +
        geom_bar(data = siBarSub, stat = "identity",
                 aes(y=aantal, fill=iscedCode.iscedNaam))+
        geom_line(data=totaalaantalselect, aes(y=aantal,  #totaal lijn
                                               group=ondCode,
                                               color="gray48")) +
        geom_point(data=totaalaantalselect, aes(y=aantal, 
                                                group=ondCode,
                                                color="gray48")) +
        geom_line(data=totaalaantal, aes(y=aantal,  #totaal lijn
                                               group=ondCode,
                                               color= "black")) + 
        geom_point(data=totaalaantal, aes(y=aantal, 
                                                group=ondCode,
                                                color="black")) +
        
        scale_color_manual(values=c("black","gray48"),breaks=c("black","gray48"), labels=c("Totaallijn","Totaallijn geselecteerde"))+
        labs(color = "Totaallijn")+
        labs(fill = "Studierichting")
      
    }
    else if (input$StudentenIngeschreven_Totaalselect == TRUE ){
      #alleen select
      
      ##keuze maken welke studies
      totaalaantalselect <- studenten_ingeschrevenen[studenten_ingeschrevenen$iscedCode.iscedNaam %in% input$StudentenIngeschreven_SelectStudyImp,]
      
      #Totaal berekenen
      totaalaantalselect <- aggregate(totaalaantalselect$aantal, by=list(ondCode=totaalaantalselect$ondCode,jaartal=totaalaantalselect$jaartal), FUN=sum)
      colnames(totaalaantalselect)<-c("ondCode","jaartal", "aantal")
      
      
      #keuze maken welk studie niveau
      
      totaalaantalselect <- switch (input$StudentenIngeschreven_StudieNiveau,
                                    "HBO" = totaalaantalselect[totaalaantalselect$ondCode == "HBO",],
                                    "WO" = totaalaantalselect[totaalaantalselect$ondCode == "WO",],
                                    "HBOWO" = aggregate(totaalaantalselect$aantal, by=list(jaartal=totaalaantalselect$jaartal), FUN=sum)
      )        
      if (input$StudentenIngeschreven_StudieNiveau == "HBOWO"){
        colnames(totaalaantalselect)<-c("jaartal","aantal")
        totaalaantalselect$ondCode = "Totaal geselecteerde HBO en WO studies"
      }
      if (input$StudentenIngeschreven_StudieNiveau == "HBO"){
        totaalaantalselect$ondCode = "Totaal geselecteerde HBO studies"
      }
      if (input$StudentenIngeschreven_StudieNiveau == "WO"){
        totaalaantalselect$ondCode = "Totaal geselecteerde WO studies"
      }
      
      ggplot(siBarSub, 
             aes(x=jaartal)) + 
        xlab("Jaar") +  
        ylab("Aantal studenten") + 
        ggtitle(plotTitle) +
        geom_bar(data = siBarSub, stat = "identity",
                 aes(y=aantal, fill=iscedCode.iscedNaam))+
        geom_line(data=totaalaantalselect, aes(y=aantal,  #totaal lijn
                                         group=ondCode,
                                         color= "gray48")) + 
        geom_point(data=totaalaantalselect, aes(y=aantal, 
                                          group=ondCode,
                                          color="gray48")) +
        scale_color_manual(values=c("gray48"),breaks=c("gray48"), labels=c("Totaallijn geselecteerde"))+
        labs(color = "Totaallijn")+
        labs(fill = "Studierichting") 
      
    } 
    else if (input$StudentenIngeschreven_Totaal == TRUE ){
      totaalaantal <- aggregate(studenten_ingeschrevenen$aantal, by=list(ondCode=studenten_ingeschrevenen$ondCode,jaartal=studenten_ingeschrevenen$jaartal), FUN=sum)
      colnames(totaalaantal)<-c("ondCode","jaartal","aantal")
      
      #keuze maken welk studie niveau
      totaalaantal <- switch (input$StudentenIngeschreven_StudieNiveau,
                              "HBO" = totaalaantal[totaalaantal$ondCode == "HBO",],
                              "WO" = totaalaantal[totaalaantal$ondCode == "WO",],
                              "HBOWO" = aggregate(totaalaantal$aantal, by=list(jaartal=totaalaantal$jaartal), FUN=sum)
      )        
      if (input$StudentenIngeschreven_StudieNiveau == "HBOWO"){
        colnames(totaalaantal)<-c("jaartal","aantal")
        totaalaantal$ondCode = "Totaal HBO en WO"
      }
      if (input$StudentenIngeschreven_StudieNiveau == "HBO"){
        totaalaantal$ondCode = "Totaal HBO"
      }
      if (input$StudentenIngeschreven_StudieNiveau == "WO"){
        totaalaantal$ondCode = "Totaal WO"
      }
      
      
      ggplot(siBarSub, 
             aes(x=jaartal)) + 
        xlab("Jaar") +  
        ylab("Aantal studenten") + 
        ggtitle(plotTitle) +
        geom_bar(data = siBarSub, stat = "identity",
                  aes(y=aantal, fill=iscedCode.iscedNaam))+
        geom_line(data=totaalaantal, aes(y=aantal,  #totaal lijn
                                          group=ondCode,
                                          color= "black")) + 
        geom_point(data=totaalaantal, aes(y=aantal, 
                                          group=ondCode,
                                          color="black")) +
        scale_color_manual(values=c("black"),breaks=c("black"), labels=c("Totaallijn"))+
        labs(color = "Totaallijn")+
        labs(fill = "Studierichting") 
        
    }  
    else{
    
    #normale enkele barplot
    ggplot(siBarSub, 
           aes(x=jaartal)) + 
      xlab("Jaar") +  
      ylab("Aantal studenten") + 
      ggtitle(plotTitle) +
      geom_bar(stat = "identity",aes(y=aantal, fill=iscedCode.iscedNaam))+
      labs(fill = "Studierichting") 
    }
    
    
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