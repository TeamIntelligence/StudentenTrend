# If the shiny package is not installed yet, install it
if(!require(shiny)) {
  install.packages("shiny")
}
library(shiny)

# If the shinydashboard package is not installed yet, install it
if(!require(shinydashboard)) {
  install.packages("shinydashboard")
}
library(shinydashboard)

# If the shiny package is not installed yet, install it
if(!require(plyr)) {
  install.packages("plyr")
}
library(plyr)

# If the shiny package is not installed yet, install it
if(!require(jsonlite)) {
  install.packages("jsonlite")
}
library(jsonlite)

# If the shiny package is not installed yet, install it
if(!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

# If the shiny package is not installed yet, install it
if(!require(data.table)) {
  install.packages("data.table")
}
library(data.table)

LoadFromServer <- function(vName, ...) {
  if(!exists(vName, envir=.GlobalEnv)) {
    assign(vName, do.call(fromJSON, list(...)), envir=.GlobalEnv)
  }
}

LoadFromServer("studievoortgang", "http://188.166.3.196:8080/studenten/studievoortgang")
LoadFromServer("studenten_gediplomeerden", "http://188.166.3.196:8080/studenten/gediplomeerden",flatten=TRUE)
LoadFromServer("vacatures", "http://188.166.3.196:8080/vacatures",flatten=TRUE)
LoadFromServer("vacatures_jaartallen", "http://188.166.3.196:8080/vacatures/jaartallen",flatten=TRUE)
LoadFromServer("studenten_ingeschrevenen", "http://188.166.3.196:8080/studenten/ingeschrevenen",flatten=TRUE)