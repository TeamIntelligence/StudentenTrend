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

#load our files
source("utils/FileLoader.R")

studievoortgang <- fromJSON("http://188.166.3.196:8080/studenten/studievoortgang")
studenten_gediplomeerden <- fromJSON("http://188.166.3.196:8080/studenten/gediplomeerden")
vacatures <- fromJSON("http://188.166.3.196:8080/vacatures")