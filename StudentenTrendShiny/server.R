
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
# Hoi Tim, groetjes Anne
  output$yearRange <- renderUI({
    if(is.null(input$voortgangType)){
      sliderInput("yearRangeSlider","Jaren", min = 1, 
                  max   = 8,
                  value = 3
                  #test van Anne
      )
    } else if (input$voortgangType == "uitschrijf"){
      sliderInput("yearRangeSlider","Jaren", min = 1, 
                  max   = 8,
                  value = 3
      )
    } else {
      sliderInput("yearRangeSlider","Jaren", min = 3, 
                  max   = 8,
                  value = 3
      )
    }
  })
  
  
  output$aantalStudentenPlot <- renderPlot({
    
    if (!is.null(input$selectStudy)){
      if(length(input$selectStudy) == 1){
        plotTitle <- paste("Aantal studenten per startjaar voor opleidings-sector", input$selectStudy)
        svSub     <- studievoortgang[which(studievoortgang$iscedCode$iscedNaam == input$selectStudy),]
        
        ggplot(svSub, aes(x=svSub$jaartal, y=svSub$aantal), environment=environment()) +
          xlab("Jaar") +
          ylab("Aantal Studenten") +
          geom_bar(stat = "identity", fill="red", alpha = 1/2)+
          ggtitle(plotTitle)
      } 
      else{
        names     <- paste(input$selectStudy, collapse = ', ')
        plotTitle <- paste("Studenten per startjaar voor:", names)
        
        if (nchar(plotTitle) > 100){
          plotTitle <- "Studenten per startjaar voor verscheidene opleidingen"
        }
        
        svSub <- studievoortgang[which(studievoortgang$iscedCode$iscedNaam == input$selectStudy),]
        
        ggplot(svSub, aes(x=svSub$jaartal, y=svSub$aantal, fill=svSub$iscedCode$iscedNaam), environment=environment()) +
          xlab("Jaar") +
          ylab("Aantal Studenten") +
          geom_bar(stat = "identity")+
          ggtitle(plotTitle) +
          scale_fill_manual(values=rainbow(length(input$selectStudy)),name="Opleidings Sector")
      }
    } 
  })
  #CHANGE
  output$voortgangsPercentages <- renderPlot({
    customMean <- function(dataIn) {
      i <- 0
      sum <- 0
      zeroes <- 0
      for(row in dataIn){
        if(row == 0){
          if(zeroes == 0){
            zeroes = zeroes + 1
            i      = i + 1
          } else {
            zeroes = zeroes + 1
          }
        } else {
          sum = sum + row
          i   = i + 1
        }
      }
      res = sum / i
      return(res)
    }
    
    if(!is.null(input$yearRangeSlider)){
      columnNames <- switch(input$voortgangType, 
                            "uitschrijf" = switch (input$yearRangeSlider,
                                                   "1" = c("iscedCode", "uitgeschreven1Jaar"),
                                                   "2" = c("iscedCode", "uitgeschreven2Jaar"),
                                                   "3" = c("iscedCode", "uitgeschreven3Jaar"),
                                                   "4" = c("iscedCode", "uitgeschreven4Jaar"),
                                                   "5" = c("iscedCode", "uitgeschreven5Jaar"),
                                                   "6" = c("iscedCode", "uitgeschreven6Jaar"),
                                                   "7" = c("iscedCode", "uitgeschreven7Jaar"),
                                                   "8" = c("iscedCode", "uitgeschreven8Jaar")
                            ),
                            "afgestudeerd" = switch(input$yearRangeSlider,
                                                    "2" = c("iscedCode", "hboGediplomeerd2Jaar", "woGediplomeerd2Jaar"),
                                                    "3" = c("iscedCode", "hboGediplomeerd3Jaar", "woGediplomeerd3Jaar"),
                                                    "4" = c("iscedCode", "hboGediplomeerd4Jaar", "woGediplomeerd4Jaar"),
                                                    "5" = c("iscedCode", "hboGediplomeerd5Jaar", "woGediplomeerd5Jaar"),
                                                    "6" = c("iscedCode", "hboGediplomeerd6Jaar", "woGediplomeerd6Jaar"),
                                                    "7" = c("iscedCode", "hboGediplomeerd7Jaar", "woGediplomeerd7Jaar"),
                                                    "8" = c("iscedCode", "hboGediplomeerd8Jaar", "woGediplomeerd8Jaar"),
                                                    "9" = c("iscedCode", "hboGediplomeerd9Jaar", "woGediplomeerd9Jaar")
                            )
      )
    
      
      svSub <- switch (input$voortgangType,
                       "uitschrijf" = aggregate (studievoortgang[, columnNames][,2], list(studievoortgang$iscedCode$iscedNaam), customMean),
                       "afgestudeerd" = aggregate (studievoortgang[, columnNames][,2] + studievoortgang[, columnNames][,3], list(studievoortgang$iscedCode$iscedNaam), customMean)
                      )
      yLim <- switch (input$voortgangType,
                      "uitschrijf" = 35,
                      "afgestudeerd" = 100
                      )
      
      colnames(svSub) <- c("sector", "mean")
      print (svSub)
      svSub$mean <- as.numeric(svSub$mean)
      
      ggplot(svSub, aes(x = svSub$sector, y = svSub$mean, fill=svSub$sector), environment=environment()) +
        xlab("Sector") +
        ylab("Percentage") +
        geom_bar(stat="identity") +
        scale_fill_manual(values=rainbow(length(svSub$sector)),name="Opleidings Sector") +
        coord_cartesian(ylim=c(0,yLim)) + 
        theme(axis.text.x=element_blank())
        
    }
  })
})




