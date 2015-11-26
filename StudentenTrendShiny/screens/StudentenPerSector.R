#The UI function for the StudentenPerSector page
StudentenPerSectorUI <- function(PageName) {
  return(
    tabItem(tabName = PageName,
      fluidRow(
         box(width=4, height = "100%", footer = "Je kan ook typen om te zoeken",
             selectInput("StudentenPerSector_selectStudy"
                         ,"Selecteer een of meerdere studiesectoren om weer te geven:"
                         ,choices = unique(studievoortgang$iscedCode$iscedNaam)
                         ,multiple = TRUE
                         ,selectize = TRUE
                         ,selected = 1
             )
         )
        ,box(width=8, height = 800, plotOutput("StudentenPerSector_aantalStudentenPlot", height=750))
      )
    )
  )
}

#The Server function for the StudentenPerSector page
StudentenPerSectorServer <- function(input, output) {
  
  output$StudentenPerSector_aantalStudentenPlot <- renderPlot({
    if (!is.null(input$StudentenPerSector_selectStudy)) {
      if(length(input$StudentenPerSector_selectStudy) == 1) {
        plotTitle <- paste("Aantal studenten per startjaar voor opleidings-sector", input$StudentenPerSector_selectStudy)
        svSub     <- studievoortgang[which(studievoortgang$iscedCode$iscedNaam == input$StudentenPerSector_selectStudy),]
        
        ggplot(svSub, aes(x=svSub$jaartal, y=svSub$aantal), environment=environment()) +
          xlab("Jaar") +
          ylab("Aantal Studenten") +
          geom_bar(stat = "identity", fill="red", alpha = 1/2)+
          ggtitle(plotTitle)
      } else {
        names     <- paste(input$StudentenPerSector_selectStudy, collapse = ', ')
        plotTitle <- paste("Studenten per startjaar voor:", names)
        
        if (nchar(plotTitle) > 100) {
          plotTitle <- "Studenten per startjaar voor verscheidene opleidingen"
        }
        
        svSub <- studievoortgang[which(studievoortgang$iscedCode$iscedNaam %in% input$StudentenPerSector_selectStudy),]
        
        ggplot(svSub, aes(x=svSub$jaartal, y=svSub$aantal, fill=svSub$iscedCode$iscedNaam), environment=environment()) +
          xlab("Jaar") +
          ylab("Aantal Studenten") +
          geom_bar(stat = "identity")+
          ggtitle(plotTitle) +
          scale_fill_manual(values=rainbow(length(input$StudentenPerSector_selectStudy)),name="Opleidings Sector")
      }
    } 
  })
}