
getSidebar <- function(){
  dashboardSidebar(
   sidebarMenu(
     menuItem("Mitarbeiter", tabName = "employees", icon = icon("th")),
     menuItem("Dummy", tabName = "employees2", icon = icon("th"))
    )
  )
}