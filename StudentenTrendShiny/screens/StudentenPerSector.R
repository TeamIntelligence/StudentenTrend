#The UI function for the StudentenPerSector page
StudentenPerSectorUI <- function(PageName) {
  return(
    tabItem(tabName = PageName,
      titlePanel("Eerstejaarsstudenten"),
      
      fluidRow(
        box(width=6, height = 170, 
            selectInput("StudentenPerSector_selectStudyImp",
                        "Selecteer een of meerdere studiesectoren om weer te geven:",
                        choices = studievoortgang$iscedCode.iscedNaam,
                        multiple = TRUE,
                        selectize = TRUE),

            
            checkboxInput("StudentenPerSector_AlleStudies",
                          "Geef alle studies weer"
            )
        ),
        
        box(width = 6, height = 170,
            checkboxInput("StudentenEerstejaars_Totaalselect",
                          "Totaal lijn weergeven van de geselecteerde studies"
            ),
            checkboxInput("StudentenEerstejaars_Totaal",
                          "Totaal lijn weergeven"
            )
            
        )
        
        # Show a plot of the generated distribution
        ,box(width=5, height = 470, plotOutput("StudentenPerSector_aantalStudentenPlot", height=450))
        ,box(width=7, height = 470, plotOutput("StudentenPerSector_aantalStudentenBarPlot", height=450))
      )
    )
  )
}

#The Server function for the StudentenPerSector page
StudentenPerSectorServer <- function(input, output, session) {
  
  output$StudentenPerSector_aantalStudentenPlot <- renderPlot({
    
    
    svSub <- studievoortgang[studievoortgang$iscedCode.iscedNaam %in% input$StudentenPerSector_selectStudyImp,]
    
    PlotTitle <- "Aantal eerstejaarsstudenten \nper jaar verdeeld per studie"
    
#     
#         ggplot(svSub, aes(x=svSub$jaartal, y=svSub$aantal), environment=environment()) +
#           xlab("Jaar") +
#           ylab("Aantal Studenten") +
#           geom_bar(stat = "identity", fill="red", alpha = 1/2)+
#           ggtitle(plotTitle)
#       
#         ggplot(svSub, aes(x=svSub$jaartal, y=svSub$aantal, fill=svSub$iscedCode$iscedNaam), environment=environment()) +
#           xlab("Jaar") +
#           ylab("Aantal Studenten") +
#           geom_bar(stat = "identity")+
#           ggtitle(plotTitle) +
#           scale_fill_manual(values=rainbow(length(input$StudentenPerSector_selectStudyImp)),name="Opleidings Sector")
#       

        if (input$StudentenEerstejaars_Totaal == TRUE & input$StudentenEerstejaars_Totaalselect == TRUE ){ 
          
          ##allebei de lijnen
          
          ##select lijn

          totaalaantalselect <- svSub[svSub$iscedCode.iscedNaam %in% input$StudentenPerSector_selectStudyImp,] #wordt imp
        
          
          #Totaal berekenen geselecteerde
          totaalaantalselect <- aggregate(totaalaantalselect$aantal, by=list(jaartal=totaalaantalselect$jaartal), FUN=sum)
          colnames(totaalaantalselect)<-c("jaartal", "aantal")
#           
          #totaallijn
          #Totaal berekenen
          totaalaantal <- aggregate(studievoortgang$aantal, by=list(jaartal=studievoortgang$jaartal), FUN=sum)
          colnames(totaalaantal)<-c("jaartal", "aantal")
          
          #plotten
          ggplot(svSub, aes(x=jaartal)) + 
            xlab("Jaar") +  
            ylab("Aantal studenten") + 
            ggtitle(PlotTitle) +
            geom_line(data=svSub, aes(y=aantal,     #lijn studies
              group=iscedCode.iscedNaam,
              color=iscedCode.iscedNaam)) + 
            geom_point(data=svSub,aes(y=aantal, 
              group=iscedCode.iscedNaam,
              color=iscedCode.iscedNaam)) +
            geom_line(data=totaalaantal, aes(y=aantal)  #totaal lijn
               , color = "black") + 
            geom_point(data=totaalaantal, aes(y=aantal) 
                , color = "black") +
            geom_line(data=totaalaantalselect, aes(y=aantal)  #totaal select lijn
               , color = "gray48") + 
            geom_point(data=totaalaantalselect, aes(y=aantal) 
              , color = "gray48") +
            labs(color = "Studierichting")+ 
            theme(legend.position="none")
          
          
        }
        
        
        ######
        else if (input$StudentenEerstejaars_Totaalselect == TRUE ){
          #alleen select
          ##select lijn
          totaalaantalselect <- svSub[svSub$iscedCode.iscedNaam %in% input$StudentenPerSector_selectStudyImp,] #wordt imp

          #Totaal berekenen geselecteerde
          totaalaantalselect <- aggregate(totaalaantalselect$aantal, by=list(jaartal=totaalaantalselect$jaartal), FUN=sum)
          colnames(totaalaantalselect)<-c("jaartal", "aantal")

          #plotten
          ggplot(svSub, aes(x=jaartal)) + 
            xlab("Jaar") +  
            ylab("Aantal studenten") + 
            ggtitle(PlotTitle) +
            geom_line(data=svSub, aes(y=aantal,     #lijn studies
                                      group=iscedCode.iscedNaam,
                                      color=iscedCode.iscedNaam)) + 
            geom_point(data=svSub,aes(y=aantal, 
                                      group=iscedCode.iscedNaam,
                                      color=iscedCode.iscedNaam)) +
            geom_line(data=totaalaantalselect, aes(y=aantal)  #totaal select lijn
                      , color = "gray48") + 
            geom_point(data=totaalaantalselect, aes(y=aantal) 
                       , color = "gray48") +
            labs(color = "Studierichting")+ 
            theme(legend.position="none")
          
          
        }
        else if (input$StudentenEerstejaars_Totaal == TRUE ){
          #totaallijn
          #Totaal berekenen
          totaalaantal <- aggregate(studievoortgang$aantal, by=list(jaartal=studievoortgang$jaartal), FUN=sum)
          colnames(totaalaantal)<-c("jaartal", "aantal")

          #totaallijn
          #Totaal berekenen
          totaalaantal <- aggregate(studievoortgang$aantal, by=list(jaartal=studievoortgang$jaartal), FUN=sum)
          colnames(totaalaantal)<-c("jaartal", "aantal")
          
          #plotten
          ggplot(svSub, aes(x=jaartal)) + 
            xlab("Jaar") +  
            ylab("Aantal studenten") + 
            ggtitle(PlotTitle) +
            geom_line(data=svSub, aes(y=aantal,     #lijn studies
                                      group=iscedCode.iscedNaam,
                                      color=iscedCode.iscedNaam)) + 
            geom_point(data=svSub,aes(y=aantal, 
                                      group=iscedCode.iscedNaam,
                                      color=iscedCode.iscedNaam)) +
            geom_line(data=totaalaantal, aes(y=aantal)  #totaal lijn
                      , color = "black") + 
            geom_point(data=totaalaantal, aes(y=aantal) 
                       , color = "black") +
            labs(color = "Studierichting")+ 
            theme(legend.position="none")
          
          
        }
        else{
          #plotten
          ggplot(svSub, aes(x=jaartal)) + 
            xlab("Jaar") +  
            ylab("Aantal studenten") + 
            ggtitle(PlotTitle) +
            geom_line(data=svSub, aes(y=aantal,     #lijn studies
                                      group=iscedCode.iscedNaam,
                                      color=iscedCode.iscedNaam)) + 
            geom_point(data=svSub,aes(y=aantal, 
                                      group=iscedCode.iscedNaam,
                                      color=iscedCode.iscedNaam)) +
            labs(color = "Studierichting")+ 
            theme(legend.position="none")
        }
        
        
        
        
  })
  
  output$StudentenPerSector_aantalStudentenBarPlot <- renderPlot({
    
    
    svBarSub <- studievoortgang[studievoortgang$iscedCode.iscedNaam %in% input$StudentenPerSector_selectStudyImp,]
    
    PlotTitle <- "Aantal eerstejaarsstudenten \nper jaar verdeeld per studie"
    
    #     
    #         ggplot(svSub, aes(x=svSub$jaartal, y=svSub$aantal), environment=environment()) +
    #           xlab("Jaar") +
    #           ylab("Aantal Studenten") +
    #           geom_bar(stat = "identity", fill="red", alpha = 1/2)+
    #           ggtitle(plotTitle)
    #       
    #         ggplot(svSub, aes(x=svSub$jaartal, y=svSub$aantal, fill=svSub$iscedCode$iscedNaam), environment=environment()) +
    #           xlab("Jaar") +
    #           ylab("Aantal Studenten") +
    #           geom_bar(stat = "identity")+
    #           ggtitle(plotTitle) +
    #           scale_fill_manual(values=rainbow(length(input$StudentenPerSector_selectStudyImp)),name="Opleidings Sector")
    #       
    
    if (input$StudentenEerstejaars_Totaal == TRUE & input$StudentenEerstejaars_Totaalselect == TRUE ){ 
      
      ##allebei de lijnen
      
      ##select lijn
      
      totaalaantalselect <- svBarSub[svBarSub$iscedCode.iscedNaam %in% input$StudentenPerSector_selectStudyImp,] #wordt imp
      
      
      #Totaal berekenen geselecteerde
      totaalaantalselect <- aggregate(totaalaantalselect$aantal, by=list(jaartal=totaalaantalselect$jaartal), FUN=sum)
      colnames(totaalaantalselect)<-c("jaartal", "aantal")
      #           
      #totaallijn
      #Totaal berekenen
      totaalaantal <- aggregate(studievoortgang$aantal, by=list(jaartal=studievoortgang$jaartal), FUN=sum)
      colnames(totaalaantal)<-c("jaartal", "aantal")
      
      #plotten
      ggplot(svBarSub, aes(x=jaartal)) + 
        xlab("Jaar") +  
        ylab("Aantal studenten") + 
        ggtitle(PlotTitle) +
        geom_bar(data=svBarSub, stat = "identity",
                                  aes(y=aantal,fill=iscedCode.iscedNaam)) +
        geom_line(data=totaalaantalselect, aes(y=aantal  #totaal select lijn
                  , color = "gray48")) + 
        geom_point(data=totaalaantalselect, aes(y=aantal 
                   , color = "gray48")) +
        geom_line(data=totaalaantal, aes(y=aantal  #totaal lijn
                  , color = "black")) + 
        geom_point(data=totaalaantal, aes(y=aantal 
                   , color = "black")) +
        scale_color_manual(values=c("black","gray48"),breaks=c("black","gray48"), labels=c("Totaallijn","Totaallijn geselecteerde"))+
        labs(color = "Totaallijn")+
        labs(fill = "Studierichting")
      
      
    }
    
    
    ######
    else if (input$StudentenEerstejaars_Totaalselect == TRUE ){
      #alleen select
      ##select lijn
      totaalaantalselect <- svBarSub[svBarSub$iscedCode.iscedNaam %in% input$StudentenPerSector_selectStudyImp,] #wordt imp
      
      #Totaal berekenen geselecteerde
      totaalaantalselect <- aggregate(totaalaantalselect$aantal, by=list(jaartal=totaalaantalselect$jaartal), FUN=sum)
      colnames(totaalaantalselect)<-c("jaartal", "aantal")
      
      #plotten
      ggplot(svBarSub, aes(x=jaartal)) + 
        xlab("Jaar") +  
        ylab("Aantal studenten") + 
        ggtitle(PlotTitle) +
        geom_bar(data=svBarSub, stat = "identity",
                 aes(y=aantal,fill=iscedCode.iscedNaam)) +
        geom_line(data=totaalaantalselect, aes(y=aantal  #totaal select lijn
                  , color = "gray48")) + 
        geom_point(data=totaalaantalselect, aes(y=aantal 
                   , color = "gray48")) +
        scale_color_manual(values=c("gray48"),breaks=c("gray48"), labels=c("Totaallijn geselecteerde"))+
        labs(color = "Totaallijn")+
        labs(fill = "Studierichting")
      
      
    }
    else if (input$StudentenEerstejaars_Totaal == TRUE ){
      #totaallijn
      #Totaal berekenen
      totaalaantal <- aggregate(studievoortgang$aantal, by=list(jaartal=studievoortgang$jaartal), FUN=sum)
      colnames(totaalaantal)<-c("jaartal", "aantal")
      
      #totaallijn
      #Totaal berekenen
      totaalaantal <- aggregate(studievoortgang$aantal, by=list(jaartal=studievoortgang$jaartal), FUN=sum)
      colnames(totaalaantal)<-c("jaartal", "aantal")
      
      #plotten
      ggplot(svBarSub, aes(x=jaartal)) + 
        xlab("Jaar") +  
        ylab("Aantal studenten") + 
        ggtitle(PlotTitle) +
        geom_bar(data=svBarSub, stat = "identity",
                 aes(y=aantal,fill=iscedCode.iscedNaam)) +
        geom_line(data=totaalaantal, aes(y=aantal  #totaal lijn
                  , color = "black")) + 
        geom_point(data=totaalaantal, aes(y=aantal 
                   , color = "black")) +
        scale_color_manual(values=c("black"),breaks=c("black"), labels=c("Totaallijn"))+
        labs(color = "Totaallijn")+
        labs(fill = "Studierichting")
      
      
    }
    else{
      #plotten
      ggplot(svBarSub, aes(x=jaartal)) + 
        xlab("Jaar") +  
        ylab("Aantal studenten") + 
        ggtitle(PlotTitle) +
        geom_bar(data=svBarSub, stat = "identity",
                 aes(y=aantal,fill=iscedCode.iscedNaam)) +
        labs(fill = "Studierichting")
    }
    
    
    
    
  })      
        
        
        
  observe({
    trueFalse = length(input$StudentenPerSector_selectStudyImp) == length(unique(studievoortgang$iscedCode.iscedNaam))

    updateCheckboxInput(session, "StudentenPerSector_AlleStudies", value = trueFalse)
    
  })
  observeEvent(input$StudentenPerSector_AlleStudies, {
    trueFalse = length(input$StudentenPerSector_selectStudyImp) == length(unique(studievoortgang$iscedCode.iscedNaam))
    if(input$StudentenPerSector_AlleStudies == T && !trueFalse){
      updateSelectInput(session, "StudentenPerSector_selectStudyImp",
                        selected = studievoortgang$iscedCode.iscedNaam
      )
    }
  })
}