StudentenGediplomeerdenUI <- function(PageName){
  return(
    
    tabItem(tabName = PageName,
            fluidRow(
              # Application title
              titlePanel("Gediplomeerde studenten"),
              box(width=4, height = "100%",
                  
                  selectInput("StudentenGediplomeerden_StudySelect",
                              "Selecteer een of meerdere studiesectoren om weer te geven:",
                              choices = studenten_gediplomeerden$iscedCode.iscedNaam,
                              multiple = TRUE,
                              selectize = TRUE,
                              selected = 1
                  ),
                  
                  checkboxInput("StudentenGediplomeerden_AlleStudies",
                                "Geef alle studies weer"
                  ),
                  
#                   checkboxInput("StudentenGediplomeerden_TotaalGeselecteerd",
#                                 "Totaal lijn weergeven van de geselecteerde studies"
#                   ),
#                   
#                   checkboxInput("StudentenGediplomeerden_Totaal",
#                                 "Totaal lijn weergeven"
#                   ),
                  
                  radioButtons("StudentenGediplomeerden_StudieNiveau",
                               "Studie Niveau", 
                               choices = list("HBO" = "HBO", 
                                              "WO Bachelor"  = "WOB",
                                              "WO Master" = "WOM",
                                              "HBO en WO Master" = "HBOWO")
                  )
                  
              )
              ,box(width=8, height = 600, plotOutput("DiploPlot", height=600))
            )
    )
  )

}

StudentenGediplomeerdenServer <- function(input, output, session){
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
        geom_line(aes(y=aantal, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam)) + 
        geom_point(aes(y=aantal, group=iscedCode.iscedNaam, color=iscedCode.iscedNaam)) +
        labs(color = "Studierichting") 
    
    
    
#     soortSub <- switch (input$StudieSoort,
#                      "HBO" = studenten_gediplomeerden[studenten_gediplomeerden$ondCode == "HBO",],
#                      "WO" = studenten_gediplomeerden[studenten_gediplomeerden$ondCode == "WO",],
#                      "HBOWO" = aggregate(studenten_gediplomeerden$aantal, by=list(iscedNaam=studenten_gediplomeerden$iscedCode.iscedNaam,jaartal=studenten_gediplomeerden$jaartal), FUN=sum)
#     )
#     
#     if(input$StudieSoort == "HBOWO"){
#       colnames(soortSub)<-c("iscedCode.iscedNaam","jaartal","aantal")
#     }
#     
#      observe({
#        if (input$Uncheck > 0){
#          updateCheckboxGroupInput(session=session,inputId="checkGroup", choices=unique(studenten_gediplomeerden$iscedCode.iscedNaam), selected=NULL)
#          
#          }
#      })
#     
#     soortSub <- soortSub[soortSub$iscedCode.iscedNaam %in% input$checkGroup,]
#     
#     
#     if (!is.null(input$checkGroup)){ #minimaal 1 studie
#       if(length(input$checkGroup) == 1){ #1studie
#         plotTitle <- paste("Aantal afgestudeerden studenten \nper jaar verdeeld per studie", input$checkGroup)
#       } 
#       else{ # meerdere studies, met namen in de titel
#         names     <- paste(input$checkGroup, collapse = ', ')
#         plotTitle <- paste("Aantal afgestudeerden studenten voor:", names)
#         
#         if (nchar(plotTitle) > 100){ #te lange naam aanpassen
#           plotTitle <- "Aantal afgestudeerden studenten voor verscheidene opleidingen"
#         }
#         
#       }
#       
#       ggplot(soortSub, aes(x=jaartal, y=aantal, fill=iscedCode.iscedNaam),environment = environment())+
#         xlab("Jaar") +  
#         ylab("Aantal gediplomeerde studenten") + 
#         ggtitle(plotTitle) +
#         geom_bar(stat = "identity")+
#         #labs(color = "Studierichting")
#         scale_fill_manual(values=rainbow(length(input$checkGroup)),name="Studierichting")
#       
#       ggplot(soortSub, aes(x=jaartal, y=aantal, group=iscedCode.iscedNaam,color=iscedCode.iscedNaam)) + 
#         xlab("Jaar") +  
#         ylab("Aantal gediplomeerde studenten") + 
#         ggtitle(plotTitle) +
#         geom_line() + 
#         geom_point() +
#         labs(color = "Studierichting")
      
      } 
  })
  

  
}