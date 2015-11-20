StudentenGediplomeerdenUI <- function(PageName){
  return(
    tabItem(tabName = PageName,
      # Application title
      titlePanel("Histogram van aantal gediplomeerden"),
      
      # Sidebar
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("checkGroup",
                             label = h3("SBI"),
                             choices = unique(studenten_gediplomeerden$iscedCode$iscedNaam),
                             selected = unique(studenten_gediplomeerden$iscedCode$iscedNaam)
          )
        ),
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("DiploPlot")
        )
      )
    )
  )
}

StudentenGediplomeerdenServer <- function(input, output){
  output$DiploPlot <- renderPlot({
    
    studentSub <- studenten_gediplomeerden[which(studenten_gediplomeerden$iscedCode$iscedNaam %in% input$checkGroup),]
    # draw the histogram
    ggplot(studentSub, aes(x=studentSub$jaartal,y=studentSub$aantal, fill=studentSub$iscedCode$iscedNaam),environment = environment()) +
      xlab("Jaar") + 
      ylab("Aantal gediplomeerden") +
      geom_bar(stat = "identity") +
      ggtitle("Aantal gediplomeerden per jaar per ISCED") +
      scale_fill_manual(values=rainbow(26),name="ISCED")
  })
  
}