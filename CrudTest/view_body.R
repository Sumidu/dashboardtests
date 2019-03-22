getBody <- function(){
  dashboardBody(
    tabItems(
    # First tab content
      tabItem(tabName = "employees",
              projectViewUI("overview")
            
      ),
      
      
    # Second tab  
      tabItem(tabName = "employees2",
              fluidRow(                
                box(width = 12,
                    h1("Employees")
                )
              )
      )
    )
  )
}