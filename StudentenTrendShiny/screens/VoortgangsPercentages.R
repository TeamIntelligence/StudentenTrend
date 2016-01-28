VoortgangsPercentagesUI <- function(PageName) {
  return(
    tabItem(tabName = PageName,
      
        fluidRow(
          box(width = 12, title = "Voortgangspercentages per studie",
              p("Op deze pagina vindt u de afstudeer- en uitschrijfpercentages per studiesector, gemiddeld genomen over de periode 1995 tot en met 2012. HBO en WO is samengenomen. U kunt zelf kiezen van hoeveel jaar na aanvang u dit percentage wilt zien. Daarnaast kunt u ook kiezen of u dit percentage ten opzichte van alle studenten wilt zien of alleen ten opzichte van alle uitgeschreven of gediplomeerde studenten wilt zien."),
              p("De grafiek biedt vervolgens inzicht hoe lang studenten over het algemeen over hun opleiding doen en hoezeer de studies voldoen aan hun verwachtingen."),
              collapsible = T
              ),
              
          
          box(width=4, height = 170, 
            radioButtons("VoortgangsPercentages_voortgangType", "Soort", 
                         choices = list("Uitschrijf percentages" = "uitschrijf", 
                                        "Afstudeer percentages"  = "afgestudeerd")
            )
        ),
        box(width=4, height = 170, sliderInput("VoortgangsPercentages_yearRangeSlider","Jaren", min = 3, max = 9, value = 3)),
        box(width=4, height = 170, uiOutput("VoortgangsPercentages_percentageRadio"))
         
        
        # Show a plot of the generated distribution
        ,box(width=12, height = 470, plotOutput("VoortgangsPercentages_plot", height=450))
      )
    )
  )
}

VoortgangsPercentagesServer <- function(input, output, session) {
  
  output$VoortgangsPercentages_percentageRadio <- renderUI({
    if(input$VoortgangsPercentages_voortgangType == "uitschrijf"){
      radioButtons("VoortgangsPercentages_check", "Percentage berekening",
                   choices = list("Percentage uitschrijvers t.o.v. alle gestarte studenten" = "normalSet",
                                  "Percentage uitschrijvers t.o.v. alle uitgeschreven studenten" = "morphSet")
      ) 
    } else {
      radioButtons("VoortgangsPercentages_check", "Percentage berekening",
                   choices = list("Percentage geslaagden t.o.v. alle gestarte studenten" = "normalSet",
                                  "Percentage geslaagden t.o.v. alle geslaagde studenten" = "morphSet")
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
      data <- dataIn
      i = 1
      j = 1
      
      for(i in 1:length(names(data))){
        if(length(grep("hbo", names(data)[i]) > 0)){
          HBOColRange <- c(i:(i+6))
          WOColRange <- c((i+7):(i+13))
          UitColRange <- c((i+14):(i+21))
          break
        }
      }
      
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

    if(!is.null(input$VoortgangsPercentages_yearRangeSlider) && !is.null(input$VoortgangsPercentages_check)){
      columnNames <- if(input$VoortgangsPercentages_voortgangType == "uitschrijf"){
                        c("iscedCode.iscedNaam", 
                          paste("uitgeschreven", input$VoortgangsPercentages_yearRangeSlider, "Jaar", sep = ""))
                      } else {
                        c("iscedCode.iscedNaam", 
                          paste("hboGediplomeerd", input$VoortgangsPercentages_yearRangeSlider, "Jaar", sep = ""), 
                          paste("woGediplomeerd", input$VoortgangsPercentages_yearRangeSlider, "Jaar", sep = ""))
                      }

      if(input$VoortgangsPercentages_check == "morphSet"){
        svSet <- morphedSet
        yLim <- 100
        plotTitle <- switch(input$VoortgangsPercentages_voortgangType,
                            "uitschrijf" = paste("Percentage van alle studenten dat uitgeschreven is binnen", input$VoortgangsPercentages_yearRangeSlider, "jaar\nt.o.v. alle uitgeschreven studenten"),
                            "afgestudeerd" = paste("Percentage van alle studenten dat geslaagd is binnen", input$VoortgangsPercentages_yearRangeSlider, "jaar\nt.o.v. alle geslaagde studenten")
        )
        
      } else {
        svSet <- studievoortgang
        yLim <- switch (input$VoortgangsPercentages_voortgangType,
                        "uitschrijf" = 35,
                        "afgestudeerd" = 100
        )
        plotTitle <- switch(input$VoortgangsPercentages_voortgangType,
                            "uitschrijf" = paste("Percentage van alle studenten dat uitgeschreven is binnen", input$VoortgangsPercentages_yearRangeSlider, "jaar\nt.o.v. alle gestarte studenten"),
                            "afgestudeerd" = paste("Percentage van alle studenten dat geslaagd is binnen", input$VoortgangsPercentages_yearRangeSlider, "jaar\nt.o.v. alle gestarte studenten")
        )
      }

      svSub <- switch (input$VoortgangsPercentages_voortgangType,
                       "uitschrijf" = aggregate (svSet[, columnNames][,2], list(svSet$iscedCode.iscedNaam), customMean),
                       "afgestudeerd" = aggregate (svSet[, columnNames][,2] + svSet[, columnNames][,3], list(svSet$iscedCode.iscedNaam), customMean)
      )
      
      colnames(svSub) <- c("sector", "mean")
      svSub$mean <- as.numeric(svSub$mean)
      
      ggplot(svSub, aes(x = svSub$sector, y = svSub$mean, fill=svSub$sector), environment=environment()) +
        xlab("Sector") +
        ylab("Percentage") +
        geom_bar(stat="identity") +
        scale_fill_manual(values=GetColors(svSub$sector),name="Studiesector") +
        coord_cartesian(ylim=c(0,yLim)) + 
        theme(axis.text.x=element_blank()) +
        ggtitle(plotTitle)
    }
  })
  observe({
    
    if(input$VoortgangsPercentages_voortgangType == "uitschrijf"){
      updateSliderInput(session = session, "VoortgangsPercentages_yearRangeSlider", value = 2,
                        min = 2, max = 8)
    } else {
      updateSliderInput(session = session, "VoortgangsPercentages_yearRangeSlider", value = 3,
                        min = 3, max = 9)
    }
    
  })
}