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
<<<<<<< HEAD
}
=======
}


TotaalAantalSelect <- function(data, selectInput, studieNiveauInput = NULL, filterParams){
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
    }
    if (studieNiveauInput == "HBO"){
      totaalaantalselect$ondCode = "Totaal geselecteerde HBO studies"
    }
    if (studieNiveauInput == "WOB"){
      totaalaantalselect$ondCode = "Totaal geselecteerde WO Bachelor studies"
    }
    if (studieNiveauInput == "WOM"){
      totaalaantalselect$ondCode = "Totaal geselecteerde WO Master studies"
    }
    if (studieNiveauInput == "WO"){
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
        totaalaantal$ondCode = "Totaal geselecteerde HBO Bachelor en WO Master studies"
      } else{
        totaalaantal$ondCode = "Totaal geselecteerde HBO en WO studies"
      }
    }
    if (studieNiveauInput == "HBO"){
      totaalaantal$ondCode = "Totaal geselecteerde HBO studies"
    }
    if (studieNiveauInput == "WOB"){
      totaalaantal$ondCode = "Totaal geselecteerde WO Bachelor studies"
    }
    if (studieNiveauInput == "WOM"){
      totaalaantal$ondCode = "Totaal geselecteerde WO Master studies"
    }
    if (studieNiveauInput == "WO"){
      totaalaantal$ondCode = "Totaal geselecteerde WO studies"
    }
  }
  
  return(totaalaantal)
}

>>>>>>> master
