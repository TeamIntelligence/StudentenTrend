AantalVacaturesUI <- function(PageName){
  return(
    
    tabItem(tabName = PageName,
            fluidRow(
            # Application title
              titlePanel("Aantal vacatures"),
              box(width=4, height = "100%",
                  
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
                  
#                   checkboxInput("AantalVacatures_TotaalGeselecteerd",
#                                 "Totaal lijn weergeven van de geselecteerde bedrijfssectoren"
#                   ),
#                   
#                   checkboxInput("AantalVacatures_Totaal",
#                                 "Totaal lijn weergeven"
#                   ),
                  
                  
              )
              ,box(width=8, height = 600, plotOutput("VacaPlot", height=600))
            )  
            
    )
  )
}

AantalVacaturesServer <- function(input,output){
  output$VacaPlot <- renderPlot({
    
    
    AantalVacatures_vacSub <<- vacatures[vacatures$sbiCode.sbiNaam %in% input$AantalVacatures_Select,]
    
    
    
    if(input$AantalVacatures_AlleSectoren == FALSE){ #als je niet alles wilt, alleen dan kijken naar de gekozen vacatures
      AantalVacatures_vacSub <- AantalVacatures_vacSub[AantalVacatures_vacSub$sbiCode.sbiNaam %in% input$AantalVacatures_Select,]
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
      geom_line(aes(y=aantal, group=sbiCode.sbiNaam, color=sbiCode.sbiNaam)) + 
      geom_point(aes(y=aantal, group=sbiCode.sbiNaam, color=sbiCode.sbiNaam)) +
      labs(color = "Bedrijfssectoren") 
    }
    
  })
}