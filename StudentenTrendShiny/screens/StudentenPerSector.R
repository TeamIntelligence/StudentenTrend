#The UI function for the StudentenPerSector page
StudentenPerSectorUI <- function(PageName) {
  return(
    tabItem(tabName = PageName,
      titlePanel("Eerstejaarsstudenten"),
      
      fluidRow(
        box(width=12, height = 170, 
            selectInput("StudentenPerSector_selectStudyImp",
                        "Selecteer een of meerdere studiesectoren om weer te geven:",
                        choices = studievoortgang$iscedCode$iscedNaam,
                        multiple = TRUE,
                        selectize = TRUE),
            
            checkboxInput("StudentenPerSector_AlleStudies",
                          "Geef alle studies weer"
            )
        )
        ,box(width=12, height = 470, plotOutput("StudentenPerSector_aantalStudentenPlot", height=450))
      )
    )
  )
}

#The Server function for the StudentenPerSector page
StudentenPerSectorServer <- function(input, output, session) {
  
  output$StudentenPerSector_aantalStudentenPlot <- renderPlot({
    if (!is.null(input$StudentenPerSector_selectStudyImp)) {
      if(length(input$StudentenPerSector_selectStudyImp) == 1) {
        plotTitle <- paste("Aantal studenten per startjaar voor opleidings-sector", input$StudentenPerSector_selectStudyImp)
        svSub     <- studievoortgang[which(studievoortgang$iscedCode$iscedNaam == input$StudentenPerSector_selectStudyImp),]
        
        ggplot(svSub, aes(x=svSub$jaartal, y=svSub$aantal), environment=environment()) +
          xlab("Jaar") +
          ylab("Aantal Studenten") +
          geom_bar(stat = "identity", fill="red", alpha = 1/2)+
          ggtitle(plotTitle)
      } else {
        names     <- paste(input$StudentenPerSector_selectStudyImp, collapse = ', ')
        plotTitle <- paste("Studenten per startjaar voor:", names)
        
        if (nchar(plotTitle) > 100) {
          plotTitle <- "Studenten per startjaar voor verscheidene opleidingen"
        }
        
        svSub <- studievoortgang[which(studievoortgang$iscedCode$iscedNaam %in% input$StudentenPerSector_selectStudyImp),]
        
        ggplot(svSub, aes(x=svSub$jaartal, y=svSub$aantal, fill=svSub$iscedCode$iscedNaam), environment=environment()) +
          xlab("Jaar") +
          ylab("Aantal Studenten") +
          geom_bar(stat = "identity")+
          ggtitle(plotTitle) +
          scale_fill_manual(values=rainbow(length(input$StudentenPerSector_selectStudyImp)),name="Opleidings Sector")
      }
    } 
  })
  observe({
    trueFalse = length(input$StudentenPerSector_selectStudyImp) == length(unique(studievoortgang$iscedCode$iscedNaam))

    updateCheckboxInput(session, "StudentenPerSector_AlleStudies", value = trueFalse)
    
  })
  observeEvent(input$StudentenPerSector_AlleStudies, {
    trueFalse = length(input$StudentenPerSector_selectStudyImp) == length(unique(studievoortgang$iscedCode$iscedNaam))
    if(input$StudentenPerSector_AlleStudies == T && !trueFalse){
      updateSelectInput(session, "StudentenPerSector_selectStudyImp",
                        selected = studievoortgang$iscedCode$iscedNaam
      )
    }
  })
}