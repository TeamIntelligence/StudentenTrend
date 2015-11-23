StudentenGediplomeerdenUI <- function(PageName){
  return(
    
    tabItem(tabName = PageName,
            fluidRow(
              # Application title
              titlePanel("Histogram van aantal gediplomeerden"),
              box(width=4, height = "100%",
                  radioButtons("StudieSoort",
                               "Soort", 
                               choices = list("HBO" = "HBO", 
                                              "WO"  = "WO",
                                              "HBO en WO" = "HBOWO")
                  ),
                  #actionButton("Uncheck", label="Uncheck"),
                  
                  checkboxGroupInput("checkGroup",
                                     label = h3("SBI"),
                                     choices = unique(studenten_gediplomeerden$iscedCode.iscedNaam),
                                     selected = unique(studenten_gediplomeerden$iscedCode.iscedNaam)
                  )
                  
              )
              ,box(width=8, height = 600, plotOutput("DiploPlot", height=600))
            )
    )
  )

}

StudentenGediplomeerdenServer <- function(input, output, session){
  output$DiploPlot <- renderPlot({
    
    soortSub <- switch (input$StudieSoort,
                     "HBO" = studenten_gediplomeerden[studenten_gediplomeerden$ondCode == "HBO",],
                     "WO" = studenten_gediplomeerden[studenten_gediplomeerden$ondCode == "WO",],
                     "HBOWO" = aggregate(studenten_gediplomeerden$aantal, by=list(iscedNaam=studenten_gediplomeerden$iscedCode.iscedNaam,jaartal=studenten_gediplomeerden$jaartal), FUN=sum)
    )
    
    if(input$StudieSoort == "HBOWO"){
      colnames(soortSub)<-c("iscedCode.iscedNaam","jaartal","aantal")
    }
    
#     observe({
#       if (input$Uncheck > 0){
#         updateCheckboxGroupInput(session=session,inputId="checkGroup", choices=unique(studenten_gediplomeerden$iscedCode.iscedNaam), selected=NULL)
#         }
#     })
    
    soortSub <- soortSub[soortSub$iscedCode.iscedNaam %in% input$checkGroup,]
    
    
    if (!is.null(input$checkGroup)){ #minimaal 1 studie
      if(length(input$checkGroup) == 1){ #1studie
        plotTitle <- paste("Aantal afgestudeerden studenten \nper jaar verdeeld per studie", input$checkGroup)
      } 
      else{ # meerdere studies, met namen in de titel
        names     <- paste(input$checkGroup, collapse = ', ')
        plotTitle <- paste("Aantal afgestudeerden studenten voor:", names)
        
        if (nchar(plotTitle) > 100){ #te lange naam aanpassen
          plotTitle <- "Aantal afgestudeerden studenten voor verscheidene opleidingen"
        }
        
      }
      
      ggplot(soortSub, aes(x=jaartal, y=aantal, fill=iscedCode.iscedNaam),environment = environment())+
        xlab("Jaar") +  
        ylab("Aantal gediplomeerde studenten") + 
        ggtitle(plotTitle) +
        geom_bar(stat = "identity")+
        #labs(color = "Studierichting")
        scale_fill_manual(values=rainbow(length(input$checkGroup)),name="Studierichting")
      
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