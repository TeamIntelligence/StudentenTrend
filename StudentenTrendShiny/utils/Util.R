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


TotaalAantalSelect <- function(data, selectInput, studieNiveauInput = NULL, filterParams) {
  ##keuze maken welke studies
  totaalaantalselect <- data[data$iscedCode.iscedNaam %in% selectInput,]
  
  filterByList <- list() # list creeeren met  jaartal en evt ondcode en diploma voor de aggregate totaal
  for(filterValue in filterParams) {
    filterByList[filterValue] = totaalaantalselect[filterValue]
  }
  
  #Totaal berekenen
  totaalaantalselect <- aggregate(totaalaantalselect$aantal, by=filterByList, FUN=sum)
  colnames(totaalaantalselect)<-append(filterParams, "aantal")
  
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
        totaalaantal$ondCode = "Totaal aantal HBO Bachelor en WO Master studies"
      } else{
        totaalaantal$ondCode = "Totaal aantal HBO en WO studies"
      }
    }
    if (studieNiveauInput == "HBO"){
      totaalaantal$ondCode = "Totaal aantal HBO studies"
    }
    if (studieNiveauInput == "WOB"){
      totaalaantal$ondCode = "Totaal aantal WO Bachelor studies"
    }
    if (studieNiveauInput == "WOM"){
      totaalaantal$ondCode = "Totaal aantal WO Master studies"
    }
    if (studieNiveauInput == "WO"){
      totaalaantal$ondCode = "Totaal aantal WO studies"
    }
  }
  
  return(totaalaantal)
}

# Make a plotly from a ggplot. Apply our defaults to this plotly
PrintGGPlotly <- function(plot, ...) {
  params <- list(p=ggplotly(plot), hovermode = "closest", titlefont=GetDefaultTitleFont(), showlegend=FALSE, ...)
  return(
    do.call(layout, params)
  )
}

#Only extract the legend from a ggplot
gg_legend <- function(plot){ 
  table <- ggplot_gtable(ggplot_build(plot)) 
  leg <- which(sapply(table$grobs, function(x) x$name) == "guide-box") 
  legend <- table$grobs[[leg]] 
  return(legend)
} 