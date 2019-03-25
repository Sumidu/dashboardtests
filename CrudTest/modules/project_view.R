# This contains the view for the projects

# Module UI function
projectViewUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    fluidRow(
      box(width = 12,
          h2("Laufzeiten"),
          plotOutput(ns("runtimes"), brush = "plotbrush"),
          uiOutput(outputId = ns("name_filter")),
          dateRangeInput(inputId = ns("date_range_sel"),
                         label = "Zeitraum auswählen:",
                         start = "2016-01-01",
                         end = "2026-12-31",
                         min = "2016-01-01",
                         max = "2056-12-31",
                         startview = "decade",
                         weekstart = 1,
                         language = "de",
                         autoclose = T,
                         separator = "bis",
                         format= "dd-mm-yy")
          ) 
      ),
    fluidRow(
      box(width = 12,
          h1("Personal"),
          rHandsontableOutput(ns("hot"))
      )
    )
  )
}

projectview <- function(input, output, session){
  values = reactiveValues()
  nameslist = reactive({
    DF = data()
    if (!is.null(DF))
      DF %>% pull(Nachname) %>% unique() %>% sort()
  })
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
    req(input$date_range_sel)
    req(input$name_filter_selection)
    if (!is.null(DF)){
      plot_personal(DF, input$date_range_sel, input$name_filter_selection) 
    }
  })
  
  output$name_filter <- renderUI({
    tagList(
      selectInput(inputId = session$ns("name_filter_selection"),
                  label = "Personen auswählen",
                  choices = nameslist(),
                  selected = head(nameslist(),5),
                  selectize = TRUE,
                  multiple = TRUE)
    )
  })
}