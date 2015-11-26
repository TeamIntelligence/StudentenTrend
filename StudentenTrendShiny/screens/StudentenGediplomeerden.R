StudentenGediplomeerdenUI <- function(PageName){
  return(
    
    tabItem(tabName = PageName, 
            fluidRow(
              # Application title
              titlePanel("Gediplomeerde studenten"),
              box(width=4, height = 150, #background = "maroon",
                  
                  selectInput("StudentenGediplomeerden_StudySelect",
                              "Selecteer een of meerdere studiesectoren om weer te geven:",
                              choices = studenten_gediplomeerden$iscedCode.iscedNaam,
                              multiple = TRUE,
                              selectize = TRUE,
                              selected = 1
                  ),
                  
                  checkboxInput("StudentenGediplomeerden_AlleStudies",
                                "Geef alle studies weer"
                  )
              ),
              box(width=4, height = 150,
                radioButtons("StudentenGediplomeerden_StudieNiveau",
                             "Studie Niveau", 
                             choices = list("HBO" = "HBO", 
                                            "WO Bachelor"  = "WOB",
                                            "WO Master" = "WOM",
                                            "HBO en WO Master" = "HBOWO")
                )
              ),
              
              box(width=4, height = 150,
                  
                  checkboxInput("StudentenGediplomeerden_TotaalGeselecteerd",
                                "Totaal lijn weergeven van de geselecteerde studies"
                  ),
                  
                  checkboxInput("StudentenGediplomeerden_Totaal",
                                "Totaal lijn weergeven"
                  )
              )
                  
            
            ,box(width=5, height = 470, plotOutput("DiploPlot", height=450))
            ,box(width=7, height = 470, plotOutput("DiploBarPlot", height=450))
            )
          )
    )

}

StudentenGediplomeerdenServer <- function(input, output){
  output$DiploPlot <- renderPlot({
    HWSet <<- studenten_gediplomeerden[(studenten_gediplomeerden$ondCode == "HBO" & studenten_gediplomeerden$diploma == "Bachelor") | (studenten_gediplomeerden$ondCode == "WO" & studenten_gediplomeerden$diploma == "Wo-master"),] 
    
    
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
    
    if(input$StudentenGediplomeerden_AlleStudies == FALSE){ #als je niet alles wilt, alleen dan kijken naar de gekozen studies
      StudentenGediplomeerden_StudieSub <- StudentenGediplomeerden_StudieSub[StudentenGediplomeerden_StudieSub$iscedCode.iscedNaam %in% input$StudentenGediplomeerden_StudySelect,]
    }
    
    #plotten en titel laten afhangen
    if ( (!is.null(input$StudentenGediplomeerden_StudySelect)) | (input$StudentenGediplomeerden_AlleStudies==TRUE) ){ #minimaal 1 studie, of alle studies
      if(length(input$StudentenGediplomeerden_StudySelect) == 1){ #1studie
        plotTitle <- paste("Aantal gediplomeerde bachelor studenten \nper jaar verdeeld per studie", input$StudentenGediplomeerden_StudySelect)
      } 
      else{ # meerdere studies, met namen in de titel
        names     <- paste(input$StudentenGediplomeerden_StudySelect, collapse = ', ')
        plotTitle <- paste("Aantal gediplomeerde bachelor studenten voor:", names)
        
        if (nchar(plotTitle) > 100){ #te lange naam aanpassen
          plotTitle <- "Aantal gediplomeerde bachelor studenten voor verscheidene opleidingen"
        }
        
      }
      
      
      ggplot(StudentenGediplomeerden_StudieSub, aes(x=jaartal)) + 
        xlab("Jaar") +  
        ylab("Aantal studenten") + 
        ggtitle(plotTitle) +
        geom_line(aes(y=aantal, color=iscedCode.iscedNaam)) + 
        geom_point(aes(y=aantal, color=iscedCode.iscedNaam)) +
        labs(color = "Studierichting")+ 
        theme(legend.position="none")
    } 
  })
  
  output$DiploBarPlot <- renderPlot({
    HWBarSet <<- studenten_gediplomeerden[(studenten_gediplomeerden$ondCode == "HBO" & studenten_gediplomeerden$diploma == "Bachelor") | (studenten_gediplomeerden$ondCode == "WO" & studenten_gediplomeerden$diploma == "Wo-master"),] 
    
    
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
    
    if(input$StudentenGediplomeerden_AlleStudies == FALSE){ #als je niet alles wilt, alleen dan kijken naar de gekozen studies
      StudentenGediplomeerden_StudieBarSub <- StudentenGediplomeerden_StudieBarSub[StudentenGediplomeerden_StudieBarSub$iscedCode.iscedNaam %in% input$StudentenGediplomeerden_StudySelect,]
    }
    
    #plotten en titel laten afhangen
    if ( (!is.null(input$StudentenGediplomeerden_StudySelect)) | (input$StudentenGediplomeerden_AlleStudies==TRUE) ){ #minimaal 1 studie, of alle studies
      if(length(input$StudentenGediplomeerden_StudySelect) == 1){ #1studie
        plotTitle <- paste("Aantal gediplomeerde bachelor studenten \nper jaar verdeeld per studie", input$StudentenGediplomeerden_StudySelect)
      } 
      else{ # meerdere studies, met namen in de titel
        names     <- paste(input$StudentenGediplomeerden_StudySelect, collapse = ', ')
        plotTitle <- paste("Aantal gediplomeerde bachelor studenten voor:", names)
        
        if (nchar(plotTitle) > 100){ #te lange naam aanpassen
          plotTitle <- "Aantal gediplomeerde bachelor studenten voor verscheidene opleidingen"
        }
        
      }
      
      
      ggplot(StudentenGediplomeerden_StudieBarSub, aes(x=jaartal)) + 
        xlab("Jaar") +  
        ylab("Aantal studenten") + 
        ggtitle(plotTitle) +
        geom_bar(stat = "identity", aes(y=aantal, fill=iscedCode.iscedNaam)) + 
        labs(color = "Studierichting")

    } 
  })
  
}  

