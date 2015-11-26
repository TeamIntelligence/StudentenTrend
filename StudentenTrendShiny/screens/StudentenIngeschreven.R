StudentenIngeschrevenUI <- function(PageName){
  return(
    tabItem(tabName = PageName,
            # Application title
            titlePanel("Ingeschreven studenten"),
            
            sidebarLayout(
              sidebarPanel(
                uiOutput("StudentenIngeschreven_SelectStudy"
                ),
                
                checkboxInput("StudentenIngeschreven_AlleStudies",
                              "Selecteer alle studies"
                ),
                
                
                checkboxInput("StudentenIngeschreven_Totaalselect",
                              "Totaal lijn weergeven van de geselecteerde studies"
                ),
                
                checkboxInput("StudentenIngeschreven_Totaal",
                              "Totaal lijn weergeven"
                ),
                
                radioButtons("StudentenIngeschreven_StudieNiveau",
                             "Studie Niveau", 
                             choices = list("HBO" = "HBO", 
                                            "WO"  = "WO",
                                            "HBO en WO" = "HBOWO")
                )
              
              ),
              
              # Show a plot of the generated distribution
              mainPanel(
                plotOutput("aantalIngeschrevenPlot")
                
              )
              
            )
            
    )
  )
}



StudentenIngeschrevenServer <- function(input, output){
  
  output$StudentenIngeschreven_SelectStudy <- renderUI({
    if(input$StudentenIngeschreven_AlleStudies == TRUE){  #Alles studies selecteren
      selectInput("StudentenIngeschreven_SelectStudyImp",
                  "Selecteer een of meerdere studiesectoren om weer te geven:",
                  choices = studenten_ingeschrevenen$iscedCode.iscedNaam,
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = studenten_ingeschrevenen$iscedCode.iscedNaam
      )
    } else { 
      selectInput("StudentenIngeschreven_SelectStudyImp",
                  "Selecteer een of meerdere studiesectoren om weer te geven:",
                  choices = studenten_ingeschrevenen$iscedCode.iscedNaam,
                  multiple = TRUE,
                  selectize = TRUE
      )
    }   
  })
    
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
                                           color=ondCode)) + 
          geom_point(data=totaalaantal, aes(y=aantal, 
                                            group=ondCode,
                                            color=ondCode)) +
          geom_line(data=totaalaantalselect, aes(y=aantal,  #totaal select lijn
                                           group=ondCode,
                                           color=ondCode)) + 
          geom_point(data=totaalaantalselect, aes(y=aantal, 
                                            group=ondCode,
                                            color=ondCode)) +
          labs(color = "Studierichting") 
        
        
      }
      else if (input$StudentenIngeschreven_Totaalselect == TRUE ){
        #alleen select
        
        ##keuze maken welke studies
        totaalaantalselect <- studenten_ingeschrevenen[studenten_ingeschrevenen$iscedCode.iscedNaam %in% input$StudentenIngeschreven_SelectStudyImp,]
        LogVar(totaalaantalselect, "totaal aantal select")
        
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
                                           color=ondCode)) + 
          geom_point(data=totaalaantalselect, aes(y=aantal, 
                                            group=ondCode,
                                            color=ondCode)) +
          labs(color = "Studierichting") 
        
        
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
                                           color=ondCode)) + 
          geom_point(data=totaalaantal, aes(y=aantal, 
                                            group=ondCode,
                                            color=ondCode)) +
          labs(color = "Studierichting") 
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
          labs(color = "Studierichting") 
      }
      
        
      
    
    } 
    
  )
  
}