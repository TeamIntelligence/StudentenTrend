# Gets all the pages that are currently within the application
GetPages <- function() {
  return(
    list(
       list("Dashboard", tabName="Dashboard", icon=icon("dashboard"))
      ,list("Studenten",icon=icon("users")
            ,menuSubItem("Eerstejaarsstudenten", tabName="StudentenPerSector", icon=icon("bar-chart"))
            ,menuSubItem("Ingeschreven studenten",tabName = "StudentenIngeschreven",icon=icon("university"))
            ,menuSubItem("Gediplomeerde studenten", tabName="StudentenGediplomeerden", icon=icon("graduation-cap")))
      ,list("Voortgangspercentages", tabName="VoortgangsPercentages", icon=icon("line-chart"))
      ,list("Aantal vacatures", tabName = "AantalVacatures", icon=icon("briefcase"))
      ,list("Vervulde banen",icon=icon("building")
            ,menuSubItem("Per bedrijfs- per studiesector",tabName = "VacaturesGediplomeerden",icon=icon("briefcase"))
            ,menuSubItem("Per studie- per bedrijfssector",tabName = "GediplomeerdenVacatures",icon=icon("university"))
            ,menuSubItem("Per studiesector per jaar",tabName = "BanenStudieSectorJaar",icon=icon("line-chart"))) 
    )
  )
}

# Builds the sidebar menu dynamically based on the items inside the GetPages() function
BuildSideBarMenu <- function() {
  MenuItems <- GetPages()
  SideBarItems <- NULL
  
  # Loop throught the pages and create an menuitem on the left hand side
  for(item in MenuItems) {
    if(is.null(SideBarItems)) {
      SideBarItems <- list(do.call(menuItem, item))
    } else {
      SideBarItems[[length(SideBarItems)+1]] <- do.call(menuItem, item)
    }
  }
  
  return(sidebarMenu(SideBarItems))
}