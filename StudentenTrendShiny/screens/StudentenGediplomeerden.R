StudentenGediplomeerdenUI <- function(PageName){
  return(
    
    tabItem(tabName = PageName, 
        # Page title
        titlePanel("Gediplomeerde studenten"),
        fluidRow(
          box(width=4, height = 170, uiOutput("StudentenGediplomeerden_SelectStudy"),
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
          )
              
        
        ,box(width=5, height = 470, plotOutput("DiploPlot", height=450))
        ,box(width=7, height = 470, plotOutput("DiploBarPlot", height=450))
        )
      )
    )
}

StudentenGediplomeerdenServer <- function(input, output){
  
  
  
  output$StudentenGediplomeerden_SelectStudy <- renderUI({
    if(input$StudentenGediplomeerden_AlleStudies == TRUE){  #Alles studies selecteren
      selectInput("StudentenGediplomeerden_SelectStudyImp",
                  "Selecteer een of meerdere studiesectoren om weer te geven:",
                  choices = studenten_gediplomeerden$iscedCode.iscedNaam,
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = studenten_gediplomeerden$iscedCode.iscedNaam
      )
    } else { 
      selectInput("StudentenGediplomeerden_SelectStudyImp",
                  "Selecteer een of meerdere studiesectoren om weer te geven:",
                  choices = studenten_gediplomeerden$iscedCode.iscedNaam,
                  multiple = TRUE,
                  selectize = TRUE
      )
    }   
  })

  
#   selectInput("StudentenGediplomeerden_SelectStudy",
#               "Selecteer een of meerdere studiesectoren om weer te geven:",
#               choices = studenten_gediplomeerden$iscedCode.iscedNaam,
#               multiple = TRUE,
#               selectize = TRUE,
#               selected = 1
#               
#   ),
  
  
  
  output$DiploPlot <- renderPlot({
    
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
      
  
      
      if (input$StudentenGediplomeerden_Totaal == TRUE & input$StudentenGediplomeerden_TotaalSelect == TRUE ){ 
        
        ##allebei de lijnen
        
        ##select lijn
        
        
        ##keuze maken welke studies: Als HBOWO: eerst dataset goed filteren en daarmee verder. Anders vanuit origineel werken
     
  
        if (input$StudentenGediplomeerden_StudieNiveau == "HBOWO"){
          totaalaantalselect <- HWSet[HWSet$iscedCode.iscedNaam %in% input$StudentenGediplomeerden_SelectStudyImp,] #wordt imp
          
        }
        else {
        totaalaantalselect <- studenten_gediplomeerden[studenten_gediplomeerden$iscedCode.iscedNaam %in% input$StudentenGediplomeerden_SelectStudyImp,] #wordt imp
         }
        
        #Totaal berekenen
        totaalaantalselect <- aggregate(totaalaantalselect$aantal, by=list(ondCode=totaalaantalselect$ondCode,jaartal=totaalaantalselect$jaartal,diploma=totaalaantalselect$diploma), FUN=sum)
        colnames(totaalaantalselect)<-c("ondCode","jaartal","diploma", "aantal")


        #keuze maken welk studie niveau
        totaalaantalselect <- switch (input$StudentenGediplomeerden_StudieNiveau,
                                      "HBO" = totaalaantalselect[totaalaantalselect$ondCode == "HBO" & totaalaantalselect$diploma == "Bachelor",] ,
                                      "WOB" = totaalaantalselect[totaalaantalselect$ondCode == "WO" & totaalaantalselect$diploma == "Bachelor",],
                                      "WOM"= totaalaantalselect[totaalaantalselect$ondCode == "WO" & totaalaantalselect$diploma == "Wo-master",],
                                      "HBOWO" = aggregate(totaalaantalselect$aantal, by=list(jaartal=totaalaantalselect$jaartal), FUN=sum)
        )                             
                                      
       #namen veranderen voor in legenda                                       
        if (input$StudentenGediplomeerden_StudieNiveau == "HBO"){
          totaalaantalselect$ondCode = "Totaal geselecteerde HBO Bachelor studies"
        }
        if (input$StudentenGediplomeerden_StudieNiveau == "WOB"){
          totaalaantalselect$ondCode = "Totaal geselecteerde WO Bachelor studies"
        }
        if (input$StudentenGediplomeerden_StudieNiveau == "WOM"){
          totaalaantalselect$ondCode = "Totaal geselecteerde WO Master studies"
        }
        if (input$StudentenGediplomeerden_StudieNiveau == "HBOWO"){
          colnames(totaalaantalselect)<-c("jaartal","aantal")
          totaalaantalselect$ondCode = "Totaal geselecteerde HBO Bachelor en WO Master studies"
        }
        
        
        #totaallijn
        #Totaal berekenen
        totaalaantal <- aggregate(studenten_gediplomeerden$aantal, by=list(ondCode=studenten_gediplomeerden$ondCode,jaartal=studenten_gediplomeerden$jaartal,diploma=studenten_gediplomeerden$diploma), FUN=sum)
        colnames(totaalaantal)<-c("ondCode","jaartal","diploma", "aantal")

        
        
        #keuze maken welk studie niveau
      
        totaalaantal <- switch (input$StudentenGediplomeerden_StudieNiveau,
                                      "HBO" = totaalaantal[totaalaantal$ondCode == "HBO" & totaalaantal$diploma == "Bachelor",] ,
                                      "WOB" = totaalaantal[totaalaantal$ondCode == "WO" & totaalaantal$diploma == "Bachelor",],
                                      "WOM"= totaalaantal[totaalaantal$ondCode == "WO" & totaalaantal$diploma == "Wo-master",],
                                      "HBOWO" = aggregate(HWSet$aantal, by=list(jaartal=HWSet$jaartal), FUN=sum)
        )
        #namen voor legenda
        if (input$StudentenGediplomeerden_StudieNiveau == "HBO"){
          totaalaantal$ondCode = "Totaal HBO Bachelor"
        }
        if (input$StudentenGediplomeerden_StudieNiveau == "WOB"){
          totaalaantal$ondCode = "Totaal WO Bachelor"
        }
        if (input$StudentenGediplomeerden_StudieNiveau == "WOM"){
          totaalaantal$ondCode = "Totaal WO Master"
        }
        if (input$StudentenGediplomeerden_StudieNiveau == "HBOWO"){
          colnames(totaalaantal)<-c("jaartal","aantal")
          totaalaantal$ondCode = "Totaal HBO Bachelor en WO Master"
        }
        
        
        
        #plotten
        
        
        ggplot(StudentenGediplomeerden_StudieSub, aes(x=jaartal)) + 
          xlab("Jaar") +  
          ylab("Aantal studenten") + 
          ggtitle(PlotTitle) +
          geom_line(data=StudentenGediplomeerden_StudieSub, aes(y=aantal,     #lijn studies
                                    group=iscedCode.iscedNaam,
                                    color=iscedCode.iscedNaam)) + 
          geom_point(data=StudentenGediplomeerden_StudieSub,aes(y=aantal, 
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
          labs(color = "Studierichting")+ 
          theme(legend.position="none")
        
        
      }
      else if (input$StudentenGediplomeerden_TotaalSelect == TRUE ){
        #alleen select
        
        ##keuze maken welke studies
        totaalaantalselect <- studenten_gediplomeerden[studenten_gediplomeerden$iscedCode.iscedNaam %in%  input$StudentenGediplomeerden_SelectStudyImp,] #wordt imp
        
        #Totaal berekenen
        totaalaantalselect <- aggregate(totaalaantalselect$aantal, by=list(ondCode=totaalaantalselect$ondCode,jaartal=totaalaantalselect$jaartal, diploma=totaalaantalselect$diploma), FUN=sum)
        colnames(totaalaantalselect)<-c("ondCode","jaartal","diploma", "aantal")
        
        
        #keuze maken welk studie niveau
        totaalaantalselect <- switch (input$StudentenGediplomeerden_StudieNiveau,
                                      "HBO" = totaalaantalselect[totaalaantalselect$ondCode == "HBO" & totaalaantalselect$diploma == "Bachelor",] ,
                                      "WOB" = totaalaantalselect[totaalaantalselect$ondCode == "WO" & totaalaantalselect$diploma == "Bachelor",],
                                      "WOM"= totaalaantalselect[totaalaantalselect$ondCode == "WO" & totaalaantalselect$diploma == "Wo-master",],
                                      "HBOWO" = aggregate(HWSet$aantal, by=list(jaartal=HWSet$jaartal), FUN=sum)
        )       
                                
        #namen veranderen voor legenda                                       
        if (input$StudentenGediplomeerden_StudieNiveau == "HBO"){
          totaalaantalselect$ondCode = "Totaal geselecteerde HBO Bachelor studies"
        }
        if (input$StudentenGediplomeerden_StudieNiveau == "WOB"){
          totaalaantalselect$ondCode = "Totaal geselecteerde WO Bachelor studies"
        }
        if (input$StudentenGediplomeerden_StudieNiveau == "WOM"){
          totaalaantalselect$ondCode = "Totaal geselecteerde WO Master studies"
        }
        if (input$StudentenGediplomeerden_StudieNiveau == "HBOWO"){
          colnames(totaalaantalselect)<-c("jaartal","aantal")
          totaalaantalselect$ondCode = "Totaal geselecteerde HBO Bachelor en WO Master studies"
        }
        
        
        ggplot(StudentenGediplomeerden_StudieSub,   
               aes(x=jaartal)) + 
          xlab("Jaar") +  
          ylab("Aantal studenten") + 
          ggtitle(PlotTitle) +
          geom_line(data=StudentenGediplomeerden_StudieSub, aes(y=aantal,     #lijn studies
                                    group=iscedCode.iscedNaam,
                                    color=iscedCode.iscedNaam)) + 
          geom_point(data=StudentenGediplomeerden_StudieSub,aes(y=aantal, 
                                    group=iscedCode.iscedNaam,
                                    color=iscedCode.iscedNaam)) +
          geom_line(data=totaalaantalselect, aes(y=aantal,  #totaal geslecteerde lijn
                                                 group=ondCode,
                                                 color=ondCode)) + 
          geom_point(data=totaalaantalselect, aes(y=aantal, 
                                                  group=ondCode,
                                                  color=ondCode)) +
          labs(color = "Studierichting")+ 
          theme(legend.position="none")
        
        
        ##############
      }
      else if (input$StudentenGediplomeerden_Totaal == TRUE ){
        #alleen totaal
        #Totaal berekenen
        totaalaantal <- aggregate(studenten_gediplomeerden$aantal, by=list(ondCode=studenten_gediplomeerden$ondCode,jaartal=studenten_gediplomeerden$jaartal,diploma=studenten_gediplomeerden$diploma), FUN=sum)
        colnames(totaalaantal)<-c("ondCode","jaartal","diploma", "aantal")
        
        #keuze maken welk studie niveau

        totaalaantal <- switch (input$StudentenGediplomeerden_StudieNiveau,
                                "HBO" = totaalaantal[totaalaantal$ondCode == "HBO" & totaalaantal$diploma == "Bachelor",] ,
                                "WOB" = totaalaantal[totaalaantal$ondCode == "WO" & totaalaantal$diploma == "Bachelor",],
                                "WOM"= totaalaantal[totaalaantal$ondCode == "WO" & totaalaantal$diploma == "Wo-master",],
                                "HBOWO" = aggregate(HWSet$aantal, by=list(jaartal=HWSet$jaartal), FUN=sum)
        )
        
        if (input$StudentenGediplomeerden_StudieNiveau == "HBO"){
          totaalaantal$ondCode = "Totaal HBO Bachelor"
        }
        if (input$StudentenGediplomeerden_StudieNiveau == "WOB"){
          totaalaantal$ondCode = "Totaal WO Bachelor"
        }
        if (input$StudentenGediplomeerden_StudieNiveau == "WOM"){
          totaalaantal$ondCode = "Totaal WO Master"
        }
        if (input$StudentenGediplomeerden_StudieNiveau == "HBOWO"){
          colnames(totaalaantal)<-c("jaartal","aantal")
          totaalaantal$ondCode = "Totaal HBO Bachelor en WO Master"
        }
        
        ggplot(StudentenGediplomeerden_StudieSub,   
               aes(x=jaartal)) + 
          xlab("Jaar") +  
          ylab("Aantal studenten") + 
          ggtitle(PlotTitle) +
          geom_line(data=StudentenGediplomeerden_StudieSub, aes(y=aantal,     #lijn studies
                                    group=iscedCode.iscedNaam,
                                    color=iscedCode.iscedNaam)) + 
          geom_point(data=StudentenGediplomeerden_StudieSub,aes(y=aantal, 
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
        ggplot(StudentenGediplomeerden_StudieSub, 
               aes(x=jaartal)) + 
          xlab("Jaar") +  
          ylab("Aantal studenten") + 
          ggtitle(PlotTitle) +
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
    
    
    

    
#     if(input$StudentenGediplomeerden_AlleStudies == FALSE){ #als je niet alles wilt, alleen dan kijken naar de gekozen studies
#       StudentenGediplomeerden_StudieBarSub <- StudentenGediplomeerden_StudieBarSub[StudentenGediplomeerden_StudieBarSub$iscedCode.iscedNaam %in% input$StudentenGediplomeerden_SelectStudyImp,]
#     }
    
    
      ggplot(StudentenGediplomeerden_StudieBarSub, aes(x=jaartal)) + 
        xlab("Jaar") +  
        ylab("Aantal studenten") + 
        ggtitle("Aantal gediplomeerde studenten") +
        geom_bar(stat = "identity", aes(y=aantal, fill=iscedCode.iscedNaam)) + 
        labs(fill = "Studierichting")

  })
  
}  

