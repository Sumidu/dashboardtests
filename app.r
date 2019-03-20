## app.R ##
library(shiny)
library(shinydashboard)
library(rhandsontable)
library(htmlwidgets)

ui <- dashboardPage(
  dashboardHeader(title = "Project Viz", dropdownMenu(type = "messages",
                                                      messageItem(
                                                        from = "Sales Dept",
                                                        message = "Sales are steady this month."
                                                      ),
                                                      messageItem(
                                                        from = "New User",
                                                        message = "How do I register?",
                                                        icon = icon("question"),
                                                        time = "13:45"
                                                      ),
                                                      messageItem(
                                                        from = "Support",
                                                        message = "The new server is ready.",
                                                        icon = icon("life-ring"),
                                                        time = "2014-12-01"
                                                      )
  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Projektauswahl", tabName = "projekte", icon = icon("th"),badgeLabel = "neu", badgeColor = "green"),
      menuItem("SAP öffnen", icon = icon("file-code-o"), 
               href = "https://github.com/rstudio/shinydashboard/")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Histogram", status = "primary", plotOutput("plot1", height = 250)),
                
                box(
                  title = "Inputs", status = "warning", collapsible = T,
                  "Box content here", br(), "More box content",
                  sliderInput("slider", "Slider input:", 1, 100, 50),
                  textInput("text", "Text input:")
                )
              ),
              fluidRow(
                infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE), 
                infoBoxOutput("progressBox2"),
                infoBoxOutput("sum")
              ),
              fluidRow(
                # Clicking this will increment the progress amount
                box(width = 4, actionButton("count", "Increment progress"))
              ),
              fluidRow(
                rHandsontableOutput("hot", width = 350)
                
                
              )
              
      ),
      
      # Second tab content
      tabItem(tabName = "projekte",
              h2("Projektauswahl")
      )
    )
  )
)

server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  values = reactiveValues()
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })

  
  data = reactive({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]]))
        DF = data.frame(ID = 1:10, verified = TRUE, reference = c("Hi"),
                        dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                        value = c(2000.30,1123.12),
                        stringsAsFactors = F)
      else
        DF = values[["DF"]]
    }
    
    values[["DF"]] = DF
    DF
  })
  
  output$hot <- renderRHandsontable({
    DF = data()
    if (!is.null(DF))
      rhandsontable(DF, width=900, height=300, stretchH="all") %>%
      hot_col("value", format = "$0.0,00", language = "de-DE") %>%
      hot_cols(colWidths = c(50,20,300,100,100), columnSorting = TRUE)
  })
  
  output$sum <- renderInfoBox({
    DF = data()
    iconstr <- ifelse(sum(DF$value, na.rm = T)>0, "thumbs-up", "thumbs-down")
    infoBox(
      "Progress", paste(sum(DF$value, na.rm = T),"€"), icon = icon(iconstr, lib = "glyphicon"),
      color = ifelse(sum(DF$value, na.rm=T)<0, "red", "green"), fill = TRUE
    )
  })
  
}

shinyApp(ui, server)