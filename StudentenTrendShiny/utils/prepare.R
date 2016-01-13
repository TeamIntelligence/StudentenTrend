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
GetColors <- function(values){
  len <- length(unique(values))
  if(len <= 1) {
    len <- 2
  }
  
  customColorArray <- c("tan4", "tomato3",
                        "violetred", "turquoise4", 
                        "wheat3", "steelblue3", 
                        "salmon", "orchid", 
                        "orange4", "limegreen", 
                        "olivedrab1", "lightgoldenrod2", 
                        "orange", "hotpink",
                        "indianred2", "khaki", 
                        "lightblue", "lightblue4",
                        "khaki4", "navyblue",
                        "lightcoral", "aquamarine",
                        "orchid4", "purple",
                        "gold", "palegreen4",
                        "seagreen1", "peru",
                        "red", "yellow4")
  
  if (len<=length(customColorArray)){
    return(customColorArray[1:len])
  }
}

GetDefaultTitleFont <- function() {
  return(list(size=12, family="Verdana", face="plain"))
}

# Load all the R packages needed
LoadPackage("shiny")
LoadPackage("shinydashboard")
LoadPackage("dplyr")
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