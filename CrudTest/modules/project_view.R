# This contains the view for the projects

# Module UI function
projectViewUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
  fluidRow(
    box(width = 12,
        h1("Personal"),
        rHandsontableOutput(ns("hot"))
    )
  ),
  fluidRow(
    box(width = 12,
        h2("Laufzeiten"),
        plotOutput(ns("runtimes")),
        sliderInput(ns("year_begin"), "Ab:",
                    min = ymd("2016-1-1"),
                    max = ymd("2056-1-1"), 
                    value = ymd("2016-1-1"), 
                    timeFormat = "%M%Y"
        ),
        sliderInput(ns("year_end"), "Ab:",
                    min = ymd("2016-1-1"),
                    max = ymd("2056-1-1"), 
                    value = ymd("2022-1-1"), 
                    timeFormat = "%Y"
        )
        ) 
  )
  )
}

projectview <- function(input, output, session){
  values = reactiveValues()
  data = reactive({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]])){
        con <- dbConnect(RSQLite::SQLite(), "mydata.sqlite")
        DF = con %>% tbl("personal") %>% as_tibble() %>% mutate_at(.vars = c("Planstelle_Begin", "Planstelle_Ende", "Zuordnungsbeginn", "Zuordnungsende"), as_date)
        #DF = data.frame(Nachname=c("Brauner"))
      }
      else
        DF = values[["DF"]]
    }
    
    values[["DF"]] = DF
    DF
  })
  
  output$hot <- renderRHandsontable({
    DF = data()
    if (!is.null(DF))
      rhandsontable(DF, height=300, readOnly = T) 
  })

  output$runtimes <- renderPlot({
    DF = data()
    if (!is.null(DF))
      plot_personal(DF, input$year_begin, input$year_end) 
  })
}