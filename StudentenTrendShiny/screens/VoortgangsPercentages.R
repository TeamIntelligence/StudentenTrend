VoortgangsPercentagesUI <- function(PageName) {
  return(
    tabItem(tabName = PageName,
      fluidRow(
        box(width=4, height = "100%", footer = "Gemiddelde afstudeer/Uitschrijf percentages per studie binnen ... jaar na aanvang",
            radioButtons("voortgangType", "Soort", 
                         choices = list("Uitschrijf percentages" = "uitschrijf", 
                                        "Afstudeer percentages"  = "afgestudeerd")
            ),
            uiOutput    ("yearRange")
        )
        ,box(width=8, height = 800, plotOutput("voortgangsPercentages", height=750))
      )
    )
  )
}

VoortgangsPercentagesServer <- function(input, output) {
  output$voortgangsPercentages <- renderPlot({
    customMean <- function(dataIn) {
      i <- 0
      sum <- 0
      zeroes <- 0
      for(row in dataIn){
        if(row == 0){
          if(zeroes == 0){
            i = i + 1
          }
          zeroes = zeroes + 1
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
}