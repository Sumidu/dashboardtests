#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(rhandsontable)
library(htmlwidgets)
library(dplyr)
library(DBI)
library(pool)

source("modules/project_view.R")
source("view_sidebar.R")
source("view_body.R")

con <- dbConnect(RSQLite::SQLite(), "mydata.sqlite")

ui <- dashboardPage(
  header = dashboardHeader(title = "Crud Test"),
  sidebar = getSidebar(),
  body = getBody()
)


server <- function(input, output, session) {
  
  callModule(projectview,"overview")
   
}

# Run the application 
shinyApp(ui = ui, server = server)

