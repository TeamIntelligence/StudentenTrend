GetPageNameSubItem <- function(SubItem) {
  
  # Try to get the SubItemName 
  tryCatch({
    if(!is.null(SubItem[[1]]["children"][["children"]][[1]]["attribs"][[1]][["data-value"]])) {
      return(SubItem[[1]]["children"][["children"]][[1]]["attribs"][[1]][["data-value"]])
    }
  }
  ,error = function(cond) {}
  )
  
  return("")
}

CallUIFunction <- function(Page) {
  if(!is.null(Page[["tabName"]])) {
    FunctionName <- paste(Page["tabName"], "UI", sep="")
    
    #Try to call the UI function, if not exist log it
    if(exists(FunctionName, envir=.GlobalEnv)) {
      Item <- do.call(FunctionName, list(Page["tabName"]) ) 
    } else {
      Item <- tabItem(tabName = Page["tabName"],
                      fluidRow(
                        box(width=12, height = 800, title = paste("De ", Page["tabName"], " pagina is niet gevonden!")
                        )
                      )
      )
      message(paste("Could not find the UI function of: ", Page["tabName"]))
    }
    return(Item)
  }
}

CallServerFunction <- function(Page, ...) {
  if(!is.null(Page[["tabName"]]) ) {
    FunctionName <- paste(Page["tabName"], "Server", sep="")
    
    #Try to call the server function, if not exist log it
    if(exists(FunctionName, envir=.GlobalEnv)) {
      do.call(FunctionName, list(...) ) 
    } else {
      message(paste("Could not find the Server function of: ", Page["tabName"]))
    }
  }
}

TotaalAantalSelect <- function(data, selectInput = NULL, studieNiveauInput = NULL, filterParams) {
  ##keuze maken welke studies
  if(!is.null(selectInput)) {
    totaalaantalselect <- data[data$iscedCode.iscedNaam %in% selectInput,]
  } else {
    totaalaantalselect <- data
  }
  
  filterByList <- list() # list creeeren met  jaartal en evt ondcode en diploma voor de aggregate totaal
  for(filterValue in filterParams) {
    filterByList[filterValue] = totaalaantalselect[filterValue]
  }
  
  #Totaal berekenen
  totaalaantalselect <- aggregate(totaalaantalselect$aantal, by=filterByList, FUN=sum)
  colnames(totaalaantalselect)<-append(filterParams, "aantal")
  totaalaantalselect$soort <- "Totaal geselecteerd"
  
  if (!is.null(studieNiveauInput)){
    
    isGedpl <- FALSE
    if(!is.null(data$diploma)){
      isGedpl <- TRUE
    }
    #keuze maken welk studie niveau
    totaalaantalselect <- switch (studieNiveauInput,
                                  "HBO" = totaalaantalselect[totaalaantalselect$ondCode == "HBO",],
                                  "WO" = totaalaantalselect[totaalaantalselect$ondCode == "WO",],
                                  "WOB" = totaalaantalselect[totaalaantalselect$ondCode == "WO" & totaalaantalselect$diploma == "Bachelor",],
                                  "WOM"= totaalaantalselect[totaalaantalselect$ondCode == "WO" & totaalaantalselect$diploma == "Wo-master",],
                                  "HBOWO" = aggregate(totaalaantalselect$aantal, by=list(jaartal=totaalaantalselect$jaartal), FUN=sum)
    )        
    if (studieNiveauInput == "HBOWO"){
      colnames(totaalaantalselect)<-c("jaartal","aantal")
      if(isGedpl){
        totaalaantalselect$ondCode = "Totaal geselecteerde HBO Bachelor en WO Master studies"
      } else{
        totaalaantalselect$ondCode = "Totaal geselecteerde HBO en WO studies"
      }
    } else if (studieNiveauInput == "HBO"){
      totaalaantalselect$ondCode = "Totaal geselecteerde HBO studies"
    } else if (studieNiveauInput == "WOB"){
      totaalaantalselect$ondCode = "Totaal geselecteerde WO Bachelor studies"
    } else if (studieNiveauInput == "WOM"){
      totaalaantalselect$ondCode = "Totaal geselecteerde WO Master studies"
    } else if (studieNiveauInput == "WO"){
      totaalaantalselect$ondCode = "Totaal geselecteerde WO studies"
    } 
  }
  
  return(totaalaantalselect)
}


TotaalAantal <- function(data, selectInput, studieNiveauInput = NULL, filterParams){
  totaalaantal<-data
  
  filterByList <- list() # list creeeren met  jaartal en evt ondcode en diploma voor de aggregate totaal
  for(filterValue in filterParams) {
    filterByList[filterValue] = totaalaantal[filterValue]
  }
  
  #Totaal berekenen
  totaalaantal <- aggregate(totaalaantal$aantal, by=filterByList, FUN=sum)
  colnames(totaalaantal)<-append(filterParams, "aantal")
  totaalaantal$soort <- "Totaal aantal"
  
  if (!is.null(studieNiveauInput)){
    
    isGedpl <- FALSE
    if(!is.null(data$diploma)){
      isGedpl <- TRUE
    }
    
    #keuze maken welk studie niveau
    totaalaantal <- switch (studieNiveauInput,
                            "HBO" = totaalaantal[totaalaantal$ondCode == "HBO",],
                            "WO" = totaalaantal[totaalaantal$ondCode == "WO",],
                            "WOB" = totaalaantal[totaalaantal$ondCode == "WO" & totaalaantal$diploma == "Bachelor",],
                            "WOM"= totaalaantal[totaalaantal$ondCode == "WO" & totaalaantal$diploma == "Wo-master",],
                            "HBOWO" = aggregate(totaalaantal$aantal, by=list(jaartal=totaalaantal$jaartal), FUN=sum)
    )        
    if (studieNiveauInput == "HBOWO"){
      colnames(totaalaantal)<-c("jaartal","aantal")
      if(isGedpl){
        totaalaantal$soort = "Totaal aantal HBO Bachelor en WO Master studies"
      } else{
        totaalaantal$soort = "Totaal aantal HBO en WO studies"
      }
    }
    if (studieNiveauInput == "HBO"){
      totaalaantal$soort = "Totaal aantal HBO studies"
    }
    if (studieNiveauInput == "WOB"){
      totaalaantal$soort = "Totaal aantal WO Bachelor studies"
    }
    if (studieNiveauInput == "WOM"){
      totaalaantal$soort = "Totaal aantal WO Master studies"
    }
    if (studieNiveauInput == "WO"){
      totaalaantal$soort = "Totaal aantal WO studies"
    }
  }
  
  return(totaalaantal)
}

AddTotaalLine <- function(plot, data, colors, fills=NULL, forecast=FALSE,  ...) {
  color_vector <- data$soort
  if(forecast) {
    color_vector <- "black"
  }
  data$color_vector <- color_vector
  
  plot <- plot +
    geom_line(data=data, ...,
              aes(y=aantal, group=soort, color="black")) + 
    geom_point(data=data, ...,
               aes(y=aantal, group=soort, color="black"))
  
  if(forecast) {
    plot <- plot +
      geom_line(data=data, linetype="dashed", ...,
                aes(y=fitted, group=soort, color=color_vector)) + 
      geom_ribbon(data=data, aes(ymin=lo80, ymax=hi80, x=jaartal, group=soort, fill="red"), alpha=.25) +
      geom_ribbon(data=data, aes(ymin=lo95, ymax=hi95, x=jaartal, group=soort, fill="darkred"), alpha=.25)
    
    fills$values <- c(fills$values, c("red", "darkred"))
    fills$labels <- c(fills$labels, c("80% Betrouwbaarheidsinterval", "95% Betrouwbaarheidsinterval"))
  }
  
  colors$values <- c(colors$values, "black")
  colors$labels <- c(colors$labels, "Totaallijn")
  
  plot <- plot +
    labs(color = "Totaallijn")
  
  return(list(plot=plot, colors=colors, fills=fills))
}

AddTotaalSelectLine <- function(plot, data, colors, fills=NULL, forecast=FALSE, ...) {
  color_vector <- data$soort
  if(forecast) {
    color_vector <- "gray48"
  }
  data$color_vector <- color_vector
  
  plot <- plot +
    geom_line(data=data, ..., 
              aes(y=aantal, group=soort, color="gray48")) + 
    geom_point(data=data, ...,
               aes(y=aantal, group=soort, color="gray48"))
  
  if(forecast) {
    plot <- plot +
      geom_line(data=data, linetype="dashed", ...,
                aes(y=fitted, group=soort, color=color_vector)) + #Add but this adds an bug in the legend
      geom_ribbon(data=data, aes(ymin=lo80, ymax=hi80, x=jaartal, group=soort, fill="blue"), alpha=.25) +
      geom_ribbon(data=data, aes(ymin=lo95, ymax=hi95, x=jaartal, group=soort, fill="darkblue"), alpha=.25)
    
    fills$values <- c(fills$values, c("blue", "darkblue"))
    fills$labels <- c(fills$labels, c("80% Betrouwbaarheidsinterval", "95% Betrouwbaarheidsinterval"))
  }
  
  
  colors$values <- c(colors$values, "gray48")
  colors$labels <- c(colors$labels, "Totaallijn geselecteerde")
  
  plot <- plot +
    labs(color = "Totaallijn")
  
  return(list(plot=plot, colors=colors, fills=fills))
}

InitGGLegend <- function() {
  #scale_color_manual options
  legend.names <- c("values", "breaks", "labels")
  legend <- setNames(vector("list", length(legend.names )), legend.names)
  
  legend$values <- NULL
  legend$breaks <- NULL
  legend$labels <- NULL
  
  return(legend)
}

# Make a plotly from a ggplot. Apply our defaults to this plotly
PrintGGPlotly <- function(plot, ...) {
  params <- list(p=ggplotly(plot), hovermode = "closest", titlefont=GetDefaultTitleFont(), ...)
  return(
    do.call(layout, params)
  )
}