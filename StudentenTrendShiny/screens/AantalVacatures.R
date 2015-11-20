AantalVacaturesUI <- function(PageName){
  return(
    tabItem(tabName = PageName,
      # Application title
      titlePanel("Histogram van aantal vacatures"),
      
      # Sidebar
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("checkGroup",
                             label = h3("SBI"),
                             choices = unique(vacatures$sbiCode$sbiNaam),
                             selected = unique(vacatures$sbiCode$sbiNaam)
          )
        ),
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("VacaPlot")
        )
      )
    )
  )
}

AantalVacaturesServer <- function(input,output){
  output$VacaPlot <- renderPlot({
    
    vacSub <- vacatures[which(vacatures$sbiCode$sbiNaam %in% input$checkGroup),]
    # draw the histogram
    ggplot(vacSub, aes(x=vacSub$jaartal,y=vacSub$aantal, fill=vacSub$sbiCode$sbiNaam),environment = environment()) +
      xlab("Jaar") + 
      ylab("Aantal vacatures") +
      geom_bar(stat = "identity") +
      ggtitle("Aantal vacatures per jaar per SBI") +
      scale_fill_manual(values=rainbow(20),name="SBI")
  })
}