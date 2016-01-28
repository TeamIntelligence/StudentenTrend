system_name <- Sys.info()['sysname']

# Load a R package. If not yet installed, install it
LoadPackage <- function(packageName, ...) {
  if(!require(packageName, character.only = T)){
    install.packages(packageName, ...)
  }
  
  library(packageName, character.only = T)
}

# Load data from the server if the data isn't already in the workspace
LoadFromServer <- function(vName, ...) {
  if(!exists(vName, envir=.GlobalEnv)) {
    assign(vName, do.call(fromJSON, list(...)), envir=.GlobalEnv)
  }
}

#Get better colors to show within the plots
GetColors <- function(values, rev=TRUE){
  unique_values <- unique(values)
  len           <- length(unique_values)
  
  customColorArray <- c("#8B5A2C", "#CD4F39", "#D14390", "#3C868C", "#CDBA96",
                        "#4F94CD", "#EE7E71", "#DA70D6", "#8B5A19", "#6BCE35",
                        "#BDF43F", "#EEDC82", "#F3A433", "#E965B2", "#EB6263",
                        "#F0E68C", "#ADD8E6", "#68838B", "#8B864E", "#082180",
                        "#ED7F80", "#7CF7D4", "#8B4789", "#A151F0", "#FCD733",
                        "#548B53", "#7CF5A0", "#CD853F", "#E93F34", "#8B8C15")
  
  if (len<=length(customColorArray)){
    colors <- customColorArray[1:len]
    blackFound <- FALSE
    grayFound  <- FALSE
    
    for(i in 1:length(unique_values)) {
      value <- unique_values[i]
      
      if(grepl("Totaal aantal", value)) {
        blackFound <- TRUE
        colors[i] <- "black"
      } else if(grepl("Totaal geselecteerd", value)) {
        grayFound <- TRUE
        colors[i] <- "gray48"
      }
    }
    
    if(grayFound && blackFound && rev) {
      colors <- replace(colors, colors=="black", "blackTemp")
      colors <- replace(colors, colors=="gray48", "black")
      colors <- replace(colors, colors=="blackTemp", "gray48")
    }
    
    return(colors)
  }
  
  return(values)
}

GetDefaultTitleFont <- function() {
  return(list(size=12, family="Verdana", face="plain"))
}

# Load all the R packages needed
LoadPackage("shiny")
LoadPackage("shinydashboard")
LoadPackage("plyr")
LoadPackage("jsonlite")
LoadPackage("ggplot2")
LoadPackage("data.table")
LoadPackage("forecast")
LoadPackage("plotly")
LoadPackage("gapminder")

# Load the data from the server
LoadFromServer("studievoortgang", "http://188.166.3.196:8080/studenten/studievoortgang",flatten=TRUE)
LoadFromServer("studenten_gediplomeerden", "http://188.166.3.196:8080/studenten/gediplomeerden",flatten=TRUE)
LoadFromServer("vacatures", "http://188.166.3.196:8080/vacatures",flatten=TRUE)
LoadFromServer("vacatures_jaartallen", "http://188.166.3.196:8080/vacatures/jaartallen",flatten=TRUE)
LoadFromServer("studenten_ingeschrevenen", "http://188.166.3.196:8080/studenten/ingeschrevenen",flatten=TRUE)
LoadFromServer("gediplomeerden_vacatures", "http://188.166.3.196:8080/studenten/vacatures",flatten=TRUE)
LoadFromServer("sbicodes93", "http://188.166.3.196:8080/sbicodes93",flatten=FALSE)
LoadFromServer("soicodes", "http://188.166.3.196:8080/soicodes",flatten=FALSE)