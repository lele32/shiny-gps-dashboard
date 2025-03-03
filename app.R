library(shiny)
library(readr)     
library(readxl)    
library(jsonlite)  
library(DT)        

# Remove file upload limit (Set to 500MB)
options(shiny.maxRequestSize = 500*1024^2)

# Define UI
ui <- fluidPage(
  titlePanel("GPS Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload GPS Data",
                accept = c(".csv", ".xlsx", ".json")),
      tags$hr(),
      uiOutput("file_info"),
      uiOutput("column_mapping"),   # Dynamic column mapping
      uiOutput("player_filter"),    # Player filter
      uiOutput("position_filter"),  # Position filter
      uiOutput("matchday_filter"),  # MatchDay filter
      uiOutput("task_filter"),      # Task filter
      uiOutput("date_filter")       # Date filter
    ),
    mainPanel(
      DTOutput("table")  # Interactive table
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Function to read data
  read_data <- reactive({
    req(input$file)  
    ext <- tools::file_ext(input$file$name)
    file_path <- input$file$datapath
    
    data <- switch(ext,
                   "csv" = read_csv(file_path, show_col_types = FALSE),
                   "xlsx" = read_excel(file_path),
                   "json" = fromJSON(file_path, flatten = TRUE),
                   stop("Unsupported file type. Please upload CSV, XLSX, or JSON."))
    
    # Clean column names
    colnames(data) <- make.names(colnames(data))
    
    return(data)
  })
  
  # Display uploaded file details
  output$file_info <- renderUI({
    req(input$file)
    HTML(paste0("<b>File Name:</b> ", input$file$name, "<br>",
                "<b>File Size:</b> ", round(input$file$size / 1024, 2), " KB"))
  })
  
  # Dynamic column mapping UI
  output$column_mapping <- renderUI({
    req(read_data())
    cols <- colnames(read_data())
    tagList(
      selectInput("player_col", "Select Player Column:", choices = cols),
      selectInput("position_col", "Select Position Column:", choices = cols),
      selectInput("matchday_col", "Select Match Day Column:", choices = cols),
      selectInput("task_col", "Select Task Column:", choices = cols),
      selectInput("date_col", "Select Date Column:", choices = cols)
    )
  })
  
  # Dynamic filtering UI (only appears after column selection)
  output$player_filter <- renderUI({
    req(read_data(), input$player_col)
    selectInput("selected_player", "Filter by Player:",
                choices = unique(read_data()[[input$player_col]]), 
                selected = unique(read_data()[[input$player_col]])[1], multiple = TRUE)
  })
  
  output$position_filter <- renderUI({
    req(read_data(), input$position_col)
    selectInput("selected_position", "Filter by Position:",
                choices = unique(read_data()[[input$position_col]]), 
                selected = unique(read_data()[[input$position_col]]), multiple = TRUE)
  })
  
  output$matchday_filter <- renderUI({
    req(read_data(), input$matchday_col)
    selectInput("selected_matchday", "Filter by Match Day:",
                choices = unique(read_data()[[input$matchday_col]]), 
                selected = unique(read_data()[[input$matchday_col]]), multiple = TRUE)
  })
  
  output$task_filter <- renderUI({
    req(read_data(), input$task_col)
    selectInput("selected_task", "Filter by Task:",
                choices = unique(read_data()[[input$task_col]]), 
                selected = unique(read_data()[[input$task_col]]), multiple = TRUE)
  })
  
  output$date_filter <- renderUI({
    req(read_data(), input$date_col)
    dateRangeInput("selected_date", "Select Date Range:",
                   start = min(as.Date(read_data()[[input$date_col]]), na.rm = TRUE),
                   end = max(as.Date(read_data()[[input$date_col]]), na.rm = TRUE))
  })
  
  # Render table with selected filters
  output$table <- renderDT({
    req(read_data(), input$player_col, input$position_col, input$matchday_col, input$task_col, input$date_col)
    data <- read_data()
    
    # Apply filters dynamically based on selected columns
    if (!is.null(input$selected_player)) {
      data <- data[data[[input$player_col]] %in% input$selected_player, ]
    }
    
    if (!is.null(input$selected_position)) {
      data <- data[data[[input$position_col]] %in% input$selected_position, ]
    }
    
    if (!is.null(input$selected_matchday)) {
      data <- data[data[[input$matchday_col]] %in% input$selected_matchday, ]
    }
    
    if (!is.null(input$selected_task)) {
      data <- data[data[[input$task_col]] %in% input$selected_task, ]
    }
    
    if (!is.null(input$selected_date)) {
      data <- data[as.Date(data[[input$date_col]]) >= input$selected_date[1] & 
                     as.Date(data[[input$date_col]]) <= input$selected_date[2], ]
    }
    
    # Handle missing values (replace NA with "-")
    data[is.na(data)] <- "-"
    
    datatable(data, options = list(pageLength = 10))
  })
}

# Run the app
shinyApp(ui, server)