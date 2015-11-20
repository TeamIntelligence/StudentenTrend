#The UI function for the StudentenPerSector page
StudentenPerSectorUI <- function(PageName) {
  return(
    tabItem(tabName = PageName,
      fluidRow(
         box(width=4, height = "100%", footer = "Je kan ook typen om te zoeken",
             selectInput("selectStudy"
                         ,"Selecteer een of meerdere studiesectoren om weer te geven:"
                         ,choices = unique(studievoortgang$iscedCode$iscedNaam)
                         ,multiple = TRUE
                         ,selectize = TRUE
                         ,selected = 1
             )
         )
        ,box(width=8, height = 800, plotOutput("aantalStudentenPlot", height=750))
      )
    )
  )
}

#The Server function for the StudentenPerSector page
StudentenPerSectorServer <- function(input, output) {
  output$yearRange <- renderUI({
    if(is.null(input$voortgangType)) {
      sliderInput("yearRangeSlider","Jaren", min = 1, max = 8, value = 3)
    } else if (input$voortgangType == "uitschrijf") {
      sliderInput("yearRangeSlider","Jaren", min = 1, max = 8, value = 3)
    } else {
      sliderInput("yearRangeSlider","Jaren", min = 3, max = 8, value = 3)
    }
  })
  
  output$aantalStudentenPlot <- renderPlot({
    if (!is.null(input$selectStudy)) {
      if(length(input$selectStudy) == 1) {
        plotTitle <- paste("Aantal studenten per startjaar voor opleidings-sector", input$selectStudy)
        svSub     <- studievoortgang[which(studievoortgang$iscedCode$iscedNaam == input$selectStudy),]
        
        ggplot(svSub, aes(x=svSub$jaartal, y=svSub$aantal), environment=environment()) +
          xlab("Jaar") +
          ylab("Aantal Studenten") +
          geom_bar(stat = "identity", fill="red", alpha = 1/2)+
          ggtitle(plotTitle)
      } else {
        names     <- paste(input$selectStudy, collapse = ', ')
        plotTitle <- paste("Studenten per startjaar voor:", names)
        
        if (nchar(plotTitle) > 100) {
          plotTitle <- "Studenten per startjaar voor verscheidene opleidingen"
        }
        
        svSub <- studievoortgang[which(studievoortgang$iscedCode$iscedNaam %in% input$selectStudy),]
        
        ggplot(svSub, aes(x=svSub$jaartal, y=svSub$aantal, fill=svSub$iscedCode$iscedNaam), environment=environment()) +
          xlab("Jaar") +
          ylab("Aantal Studenten") +
          geom_bar(stat = "identity")+
          ggtitle(plotTitle) +
          scale_fill_manual(values=rainbow(length(input$selectStudy)),name="Opleidings Sector")
      }
    } 
  })
}