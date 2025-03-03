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
      uiOutput("column_select"),    # Column selection
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
  
  # Dynamic column selection (Fixed)
  output$column_select <- renderUI({
    req(read_data())
    selectInput("selected_columns", "Select Columns to Display:",
                choices = colnames(read_data()), 
                selected = colnames(read_data()), multiple = TRUE)
  })
  
  # Dynamic player filtering
  output$player_filter <- renderUI({
    req(read_data())
    if ("Player" %in% colnames(read_data())) {
      selectInput("selected_player", "Filter by Player:",
                  choices = unique(read_data()$Player), 
                  selected = unique(read_data()$Player)[1], multiple = TRUE)
    }
  })
  
  # Dynamic position filtering
  output$position_filter <- renderUI({
    req(read_data())
    if ("Position" %in% colnames(read_data())) {
      selectInput("selected_position", "Filter by Position:",
                  choices = unique(read_data()$Position), 
                  selected = unique(read_data()$Position), multiple = TRUE)
    }
  })
  
  # Dynamic matchday filtering
  output$matchday_filter <- renderUI({
    req(read_data())
    if ("MatchDay" %in% colnames(read_data())) {
      selectInput("selected_matchday", "Filter by Match Day:",
                  choices = unique(read_data()$MatchDay), 
                  selected = unique(read_data()$MatchDay), multiple = TRUE)
    }
  })
  
  # Dynamic task filtering
  output$task_filter <- renderUI({
    req(read_data())
    if ("Task" %in% colnames(read_data())) {
      selectInput("selected_task", "Filter by Task:",
                  choices = unique(read_data()$Task), 
                  selected = unique(read_data()$Task), multiple = TRUE)
    }
  })
  
  # Dynamic date filtering
  output$date_filter <- renderUI({
    req(read_data())
    if ("Date" %in% colnames(read_data())) {
      dateRangeInput("selected_date", "Select Date Range:",
                     start = min(read_data()$Date, na.rm = TRUE),
                     end = max(read_data()$Date, na.rm = TRUE))
    }
  })
  
  # Render table with selected filters
  output$table <- renderDT({
    req(read_data())  
    data <- read_data()
    
    # Apply column selection
    if (!is.null(input$selected_columns)) {
      data <- data[, input$selected_columns, drop = FALSE]
    }
    
    # Apply player filter
    if (!is.null(input$selected_player) && "Player" %in% colnames(data)) {
      data <- data[data$Player %in% input$selected_player, ]
    }
    
    # Apply position filter
    if (!is.null(input$selected_position) && "Position" %in% colnames(data)) {
      data <- data[data$Position %in% input$selected_position, ]
    }
    
    # Apply match day filter
    if (!is.null(input$selected_matchday) && "MatchDay" %in% colnames(data)) {
      data <- data[data$MatchDay %in% input$selected_matchday, ]
    }
    
    # Apply task filter
    if (!is.null(input$selected_task) && "Task" %in% colnames(data)) {
      data <- data[data$Task %in% input$selected_task, ]
    }
    
    # Apply date filter
    if (!is.null(input$selected_date) && "Date" %in% colnames(data)) {
      data <- data[data$Date >= input$selected_date[1] & data$Date <= input$selected_date[2], ]
    }
    
    # Handle missing values (replace NA with "-")
    data[is.na(data)] <- "-"
    
    datatable(data, options = list(pageLength = 10))
  })
}

# Run the app
shinyApp(ui, server)