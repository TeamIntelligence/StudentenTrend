StudentenIngeschrevenUI <- function(PageName){
  return(
    tabItem(tabName = PageName,
            # Application title
            titlePanel("Ingeschreven studenten"),
            
            sidebarLayout(
              sidebarPanel(
                selectInput("SelectStudy",
                            "Selecteer een of meerdere studiesectoren om weer te geven:",
                            choices = studenten_ingeschrevenen$iscedCode.iscedNaam,
                            multiple = TRUE,
                            selectize = TRUE,
                            selected = 1
                ),
                
                checkboxInput("Alles",
                              "Geef alle studies weer"
                ),
                
                checkboxInput("Totaalselect",
                              "Totaal lijn weergeven van de geselecteerde studies"
                ),
                
                checkboxInput("Totaal",
                              "Totaal lijn weergeven"
                ),
                
                radioButtons("StudieNiveau",
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
  
  output$aantalIngeschrevenPlot <- renderPlot({
    
    #data aanpassen nav keuzes gebruiker
    siSub <- switch (input$StudieNiveau,
                     "HBO" = studenten_ingeschrevenen[studenten_ingeschrevenen$ondCode == "HBO",],
                     "WO" = studenten_ingeschrevenen[studenten_ingeschrevenen$ondCode == "WO",],
                     "HBOWO" = aggregate(studenten_ingeschrevenen$aantal, by=list(iscedNaam=studenten_ingeschrevenen$iscedCode.iscedNaam,jaartal=studenten_ingeschrevenen$jaartal), FUN=sum)
    )
    
    #namen kolomtitels van de nieuwe gevormde data aanpassen
    if(input$StudieNiveau == "HBOWO"){
      colnames(siSub)<-c("iscedCode.iscedNaam","jaartal","aantal")
    }
    
    if(input$Alles == FALSE){ #als je niet alles wilt, alleen dan kijken naar de gekozen studies
      siSub <- siSub[siSub$iscedCode.iscedNaam %in% input$SelectStudy,]
    }
    
    #plotten en titel laten afhangen
    if ( (!is.null(input$SelectStudy)) | (input$Alles==TRUE) ){ #minimaal 1 studie, of alle studies
      if(length(input$SelectStudy) == 1){ #1studie
        plotTitle <- paste("Aantal ingeschreven bachelor studenten \nper jaar verdeeld per studie", input$SelectStudy)
      } 
      else{ # meerdere studies, met namen in de titel
        names     <- paste(input$SelectStudy, collapse = ', ')
        plotTitle <- paste("Aantal ingeschreven bachelor studenten voor:", names)
        
        if (nchar(plotTitle) > 100){ #te lange naam aanpassen
          plotTitle <- "Aantal ingeschreven bachelor studenten voor verscheidene opleidingen"
        }
        
      }
      
#       if(input$totaal == FALSE){
        
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
        
#       }
#       else{
#         totaalaantal <- aggregate(studenten_ingeschrevenen$aantal, by=list(ondCode=studenten_ingeschrevenen$ondCode,jaartal=studenten_ingeschrevenen$jaartal), FUN=sum)
#         
#         ggplot(siSub, 
#                aes(x=jaartal)) + 
#           xlab("Jaar") +  
#           ylab("Aantal studenten") + 
#           ggtitle(plotTitle) +
#           geom_line(aes(y=aantal, 
#                         group=iscedCode.iscedNaam,
#                         color=iscedCode.iscedNaam)) + 
#           geom_point(aes(y=aantal, 
#                          group=iscedCode.iscedNaam,
#                          color=iscedCode.iscedNaam)) +
#           geom_line(aes(y=totaalaantal, 
#                         group=ondCode,
#                         color=ondCode)) + 
#           geom_point(aes(y=totaalaantal, 
#                          group=ondCode,
#                          color=ondCode)) +
#           labs(color = "Studierichting") 
#       }
      
    } 
    
  })
  
}