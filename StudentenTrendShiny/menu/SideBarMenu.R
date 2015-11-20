#Gets all the pages that are currently within the application
GetPages <- function() {
  return(
    list(
       list("Dashboard", tabName="Dashboard", icon=icon("dashboard"))
      ,list("Studenten per opleidings-sector", tabName="StudentenPerSector", icon=icon("bar-chart"))
      ,list("Haalbaarheid", tabName="VoortgangsPercentages", icon=icon("bar-chart"))
      ,list("Gediplomeerde studenten", tabName="StudentenGediplomeerden", icon=icon("bar-chart"))
      ,list("Aantal vacatures", tabName = "AantalVacatures", icon=icon("bar-chart"))
      ,list("Ingeschreven studenten",tabName = "StudentenIngeschreven",icon=icon("bar-chart"))
      )
  )
}

#Builds the sidebar menu dynamically based on the items inside the GetPages() function
BuildSideBarMenu <- function() {
  MenuItems <- GetPages()
  SideBarItems <- NULL
  
  #Loop throught the pages and create an menuitem on the left hand side
  for(item in MenuItems) {
    if(is.null(SideBarItems)) {
      SideBarItems <- list(do.call(menuItem, item))
    } else {
      SideBarItems[[length(SideBarItems)+1]] <- do.call(menuItem, item)
    }
  }
  
  return(sidebarMenu(SideBarItems))
}