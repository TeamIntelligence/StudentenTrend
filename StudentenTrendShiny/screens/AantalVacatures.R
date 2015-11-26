AantalVacaturesUI <- function(PageName){
  return(
    
    tabItem(tabName = PageName,
            fluidRow(
            # Application title
              titlePanel("Aantal vacatures"),
              box(width=5, height = 150,
                  
                  selectInput("AantalVacatures_Select",
                              "Selecteer een of meerdere bedrijfssectoren om weer te geven:",
                              choices = vacatures$sbiCode.sbiNaam,
                              multiple = TRUE,
                              selectize = TRUE,
                              selected = 1
                  ),
                  
                  checkboxInput("AantalVacatures_AlleSectoren",
                                "Geef alle bedrijfssectoren weer"
                  )
                  
              ),
              box(width=7, height=150,
                  checkboxInput("AantalVacatures_TotaalGeselecteerd",
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
  output$VacaPlot <- renderPlot({
    
    
    
    AantalVacatures_vacSub <<- vacatures_jaartallen
    
    
    if(input$AantalVacatures_AlleSectoren == FALSE){ #als je niet alles wilt, alleen dan kijken naar de gekozen vacatures
      AantalVacatures_vacSub <<- vacatures_jaartallen[vacatures_jaartallen$sbiCode.sbiNaam %in% input$AantalVacatures_Select,]
    }
    
    #plotten en titel laten afhangen
    if ( (!is.null(input$AantalVacatures_Select)) | (input$AantalVacatures_AlleSectoren==TRUE) ){ #minimaal 1 vacature, of alle vacatures
      if(length(input$AantalVacatures_Select) == 1){ #1vacature
        plotTitle <- paste("Aantal vacatures \nper jaar verdeeld per bedrijfssector", input$AantalVacatures_Select)
      } 
      else{ # meerdere vacatures, met namen in de titel
        names     <- paste(input$AantalVacatures_Select, collapse = ', ')
        plotTitle <- paste("Aantal vacatures voor:", names)
        
        if (nchar(plotTitle) > 100){ #te lange naam aanpassen
          plotTitle <- "Aantal vacatures voor verscheidene bedrijfssectoren"
        }
        
      }
    
    
    # draw the histogram
    ggplot(AantalVacatures_vacSub, aes(x=jaartal)) +
      xlab("Jaar") + 
      ylab("Aantal vacatures") +
      ggtitle(plotTitle) +
      geom_line(aes(y=aantal, color=sbiCode.sbiNaam))+
      geom_point(aes(y=aantal, color=sbiCode.sbiNaam))+ 
      theme(legend.position="none")
    }
    
  })
  output$VacaBarPlot <- renderPlot({
    
    
    
    AantalVacatures_vacBarSub <<- vacatures_jaartallen
    
    
    if(input$AantalVacatures_AlleSectoren == FALSE){ #als je niet alles wilt, alleen dan kijken naar de gekozen vacatures
      AantalVacatures_vacBarSub <<- vacatures_jaartallen[vacatures_jaartallen$sbiCode.sbiNaam %in% input$AantalVacatures_Select,]
    }
    
    #plotten en titel laten afhangen
    if ( (!is.null(input$AantalVacatures_Select)) | (input$AantalVacatures_AlleSectoren==TRUE) ){ #minimaal 1 vacature, of alle vacatures
      if(length(input$AantalVacatures_Select) == 1){ #1vacature
        plotTitle <- paste("Aantal vacatures \nper jaar verdeeld per bedrijfssector", input$AantalVacatures_Select)
      } 
      else{ # meerdere vacatures, met namen in de titel
        names     <- paste(input$AantalVacatures_Select, collapse = ', ')
        plotTitle <- paste("Aantal vacatures voor:", names)
        
        if (nchar(plotTitle) > 100){ #te lange naam aanpassen
          plotTitle <- "Aantal vacatures voor verscheidene bedrijfssectoren"
        }
        
      }
      
      # draw the histogram
      ggplot(AantalVacatures_vacBarSub, aes(x=jaartal)) +
        xlab("Jaar") + 
        ylab("Aantal vacatures") +
        ggtitle(plotTitle) +
        geom_bar(stat = "identity", aes(y=aantal, fill=sbiCode.sbiNaam)) + 
        labs(fill = "Bedrijfssectoren") 
    }
    
  })
}