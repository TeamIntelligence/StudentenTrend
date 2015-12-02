VoortgangsPercentagesUI <- function(PageName) {
  return(
    tabItem(tabName = PageName,
      titlePanel("Voortgangspercentages per studie"),
      
      fluidRow(
        box(width=4, height = 170, footer = "Gemiddelde afstudeer/uitschrijf percentages per studie van HBO en WO binnen ... jaar na aanvang",
            radioButtons("VoortgangsPercentages_voortgangType", "Soort", 
                         choices = list("Uitschrijf percentages" = "uitschrijf", 
                                        "Afstudeer percentages"  = "afgestudeerd")
            )
        ),
        box(width=4, height = 170, uiOutput("VoortgangsPercentages_yearRange")),
        box(width=4, height = 170,uiOutput("VoortgangsPercentages_percentageRadio"))
         
        
        # Show a plot of the generated distribution
        ,box(width=12, height = 470, plotOutput("VoortgangsPercentages_plot", height=450))
      )
    )
  )
}

VoortgangsPercentagesServer <- function(input, output, session) {
  
  
  output$VoortgangsPercentages_yearRange <- renderUI({
    if(is.null(input$VoortgangsPercentages_voortgangType)) {
      sliderInput("VoortgangsPercentages_yearRangeSlider","Jaren", min = 1, max = 8, value = 3)
    } else if (input$VoortgangsPercentages_voortgangType == "uitschrijf") {
      sliderInput("VoortgangsPercentages_yearRangeSlider","Jaren", min = 1, max = 8, value = 3)
    } else {
      sliderInput("VoortgangsPercentages_yearRangeSlider","Jaren", min = 3, max = 8, value = 3)
    }
  })
  
  output$VoortgangsPercentages_percentageRadio <- renderUI({
    if(input$VoortgangsPercentages_voortgangType == "uitschrijf"){
      radioButtons("VoortgangsPercentages_check", "Percentage berekening",
                   choices = list("Percentage uitschrijvers tov. alle gestarte studenten" = "normalSet",
                                  "Percentage uitschrijvers tov. alle uitgeschreven studenten" = "morphSet")
      ) 
    } else {
      radioButtons("VoortgangsPercentages_check", "Percentage berekening",
                   choices = list("Percentage geslaagden tov. alle gestarte studenten" = "normalSet",
                                  "Percentage geslaagden tov. alle geslaagde studenten" = "morphSet")
      )
    }
  })
  
  output$VoortgangsPercentages_plot <- renderPlot({
    customMean <- function(dataIn) {
      i <- 0
      sum <- 0
      zeroes <- 0
      for(row in dataIn){
        if(row == 0){
          next
        } else {
          sum = sum + row
          i   = i + 1
        }
      }
      res = sum / i
      return(res)
    }
    
    morphSet <- function(dataIn){
      data <- studievoortgang
      i = 1
      j = 1
      
      HBOColRange <- c(5:11)
      WOColRange <- c(12:18)
      UitColRange <- c(19:26)
      
      for(i in 1:nrow(data)){
        for (index in max(HBOColRange) : min(HBOColRange)){
          if(data[i, index] != 0){
            HBOWOMax <- data[i, index] + data[i, index+7]
            break
          }
        }
        for (index in max(UitColRange) : min(UitColRange)){
          if(data[i, index] != 0){
            UitMax <- data[i, index]
            break
          }
        }
        
        for(j in 1:ncol(data)){
          if(j %in% HBOColRange){
            data[i, j] <- round((data[i, j] * 100)/HBOWOMax)
          } else if (j %in% WOColRange){
            data[i, j] <- round((data[i, j] * 100)/HBOWOMax)
          } else if (j %in% UitColRange){
            data[i, j] <- round((data[i, j] * 100)/UitMax)
          }
          
        }
      }
      return(data)
    }
    if(!exists("morphedSet") || is.null(morphedSet)){
      morphedSet <- morphSet(studievoortgang)
    }    
    
    if(!is.null(input$VoortgangsPercentages_yearRangeSlider)){
      columnNames <- switch(input$VoortgangsPercentages_voortgangType, 
                            "uitschrijf" = switch (input$VoortgangsPercentages_yearRangeSlider,
                                                   "1" = c("iscedCode", "uitgeschreven1Jaar"),
                                                   "2" = c("iscedCode", "uitgeschreven2Jaar"),
                                                   "3" = c("iscedCode", "uitgeschreven3Jaar"),
                                                   "4" = c("iscedCode", "uitgeschreven4Jaar"),
                                                   "5" = c("iscedCode", "uitgeschreven5Jaar"),
                                                   "6" = c("iscedCode", "uitgeschreven6Jaar"),
                                                   "7" = c("iscedCode", "uitgeschreven7Jaar"),
                                                   "8" = c("iscedCode", "uitgeschreven8Jaar")
                            ),
                            "afgestudeerd" = switch(input$VoortgangsPercentages_yearRangeSlider,
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
      
      if(input$VoortgangsPercentages_check == "morphSet"){
        svSet <- morphedSet
        
        yLim <- 100
        
      } else {
        svSet <- studievoortgang
        yLim <- switch (input$VoortgangsPercentages_voortgangType,
                        "uitschrijf" = 35,
                        "afgestudeerd" = 100
        )
      }

      svSub <- switch (input$VoortgangsPercentages_voortgangType,
                       "uitschrijf" = aggregate (svSet[, columnNames][,2], list(svSet$iscedCode$iscedNaam), customMean),
                       "afgestudeerd" = aggregate (svSet[, columnNames][,2] + svSet[, columnNames][,3], list(svSet$iscedCode$iscedNaam), customMean)
      )
      
      
      plotTitle <- switch(input$VoortgangsPercentages_voortgangType,
                          "uitschrijf" = paste("Percentage van alle studenten dat uitgeschreven is na", input$VoortgangsPercentages_yearRangeSlider, "jaar"),
                          "afgestudeerd" = paste("Percentage van alle studenten dat geslaagd is na", input$VoortgangsPercentages_yearRangeSlider, "jaar")
      )
      
      colnames(svSub) <- c("sector", "mean")
      svSub$mean <- as.numeric(svSub$mean)
      
      ggplot(svSub, aes(x = svSub$sector, y = svSub$mean, fill=svSub$sector), environment=environment()) +
        xlab("Sector") +
        ylab("Percentage") +
        geom_bar(stat="identity") +
        scale_fill_manual(values=rainbow(length(svSub$sector)),name="Opleidings Sector") +
        coord_cartesian(ylim=c(0,yLim)) + 
        theme(axis.text.x=element_blank()) +
        ggtitle(plotTitle)
    }
  })
}