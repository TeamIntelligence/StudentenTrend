AantalVacaturesUI <- function(PageName){
  return(
    
    tabItem(tabName = PageName,
      # Page title
      titlePanel("Aantal vacatures"),
      fluidRow(
        box(width=5, height = 150, 
            
            uiOutput("AantalVacatures_Select"),
            
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
            
        )
        ,box(width=5, height = 470, plotOutput("VacaPlot", height = 450))
        ,box(width=7, height = 470, plotOutput("VacaBarPlot", height=450))
      )   
    )
  )
}

AantalVacaturesServer <- function(input,output){
  
  
  output$AantalVacatures_Select <- renderUI({
    if(input$AantalVacatures_AlleSectoren == TRUE){  #Alles bedrijfssectoren selecteren
      selectInput("AantalVacatures_SelectImp",
                  "Selecteer een of meerdere bedrijfssectoren om weer te geven:",
                  choices = vacatures$sbiCode.sbiNaam,
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = vacatures$sbiCode.sbiNaam
      )
    } else { 
      selectInput("AantalVacatures_SelectImp",
                  "Selecteer een of meerdere bedrijfssectoren om weer te geven:",
                  choices = vacatures$sbiCode.sbiNaam,
                  multiple = TRUE,
                  selectize = TRUE
      )
    } 
  })

  output$VacaPlot <- renderPlot({
    
    #data aanpassen nav keuze bedrijfssector
    AantalVacatures_vacSub <- vacatures_jaartallen[vacatures_jaartallen$sbiCode.sbiNaam %in% input$AantalVacatures_SelectImp,]
    
    ##select lijn
    #Totaal berekenen
    totaalaantalselect <- aggregate(AantalVacatures_vacSub$aantal, by=list(jaartal=AantalVacatures_vacSub$jaartal), FUN=sum)
    colnames(totaalaantalselect)<-c("jaartal", "aantal")
    totaalaantalselect$soort = "Totale geselecteerde vacatures"
    
    #totaallijn
    #Totaal berekenen
    totaalaantal <- aggregate(vacatures_jaartallen$aantal, by=list(jaartal=vacatures_jaartallen$jaartal), FUN=sum)
    colnames(totaalaantal)<-c("jaartal","aantal") 
    totaalaantal$soort = "Totale vacatures" 
    
     if (input$AantalVacatures_Totaal == TRUE & input$AantalVacatures_TotaalSelect == TRUE ){ 
       
       ##allebei de lijnen

       ggplot(AantalVacatures_vacSub, aes(x=jaartal)) +
         xlab("Jaar") + 
         ylab("Aantal vacatures") +
         ggtitle("Aantal vacatures per sector") +
         geom_line(data=vacatures_jaartallen, #normale lijnen
                   aes(y=aantal,
                       color=sbiCode.sbiNaam))+
         geom_point(data=vacatures_jaartallen,
                    aes(y=aantal, 
                        color=sbiCode.sbiNaam))+ 
         geom_line(data=totaalaantal, aes(y=aantal,  #totaal lijn
                                          group=soort,
                                          color=soort)) + 
         geom_point(data=totaalaantal, aes(y=aantal, 
                                           group=soort,
                                           color=soort)) +
         geom_line(data=totaalaantalselect, aes(y=aantal,  #totaal select lijn
                                                group=soort,
                                                color=soort)) + 
         geom_point(data=totaalaantalselect, aes(y=aantal, 
                                                 group=soort,
                                                 color=soort)) +
         theme(legend.position="none")
     }
     else if (input$AantalVacatures_TotaalSelect == TRUE ){
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
         ggtitle("Aantal vacatures per sector") +
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
         labs(color = "Studierichting")+ 
         theme(legend.position="none")
       
       
       ##############
     }
     else if (input$AantalVacatures_Totaal == TRUE ){
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
         ggtitle("Aantal vacatures per sector") +
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
         labs(color = "Studierichting")+ 
         theme(legend.position="none")
     }
     else{
       #normale enkele plot
       ggplot(AantalVacatures_vacSub, aes(x=jaartal)) +
         xlab("Jaar") + 
         ylab("Aantal vacatures") +
         ggtitle("Aantal vacatures per sector") +
         geom_line(aes(y=aantal,
                       color=sbiCode.sbiNaam))+
         geom_point(aes(y=aantal, 
                        color=sbiCode.sbiNaam))+ 
         theme(legend.position="none")
     }
     ################################

    # draw the line
    ggplot(AantalVacatures_vacSub, aes(x=jaartal)) +
      xlab("Jaar") + 
      ylab("Aantal vacatures") +
      ggtitle("Aantal vacatures per sector") +
      geom_line(aes(y=aantal, color=sbiCode.sbiNaam))+
      geom_point(aes(y=aantal, color=sbiCode.sbiNaam))+ 
      theme(legend.position="none")
    
  })
  
  output$VacaBarPlot <- renderPlot({

    AantalVacatures_vacBarSub <- vacatures_jaartallen[vacatures_jaartallen$sbiCode.sbiNaam %in% input$AantalVacatures_SelectImp,]

    

      
      # draw the histogram
      ggplot(AantalVacatures_vacBarSub, aes(x=jaartal)) +
        xlab("Jaar") + 
        ylab("Aantal vacatures") +
        ggtitle("Aantal vacatures per sector") +
        geom_bar(stat = "identity", aes(y=aantal, fill=sbiCode.sbiNaam)) + 
        labs(fill = "Bedrijfssectoren") 
    
  })
}