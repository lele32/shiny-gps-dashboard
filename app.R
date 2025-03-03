library(shiny)
library(readr)     
library(readxl)    
library(jsonlite)  
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)

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
      uiOutput("date_filter"),      # Date filter
      selectInput("metric", "Select Metric:", choices = NULL)  # ✅ Metric selection
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DTOutput("table")),
        tabPanel("Summary Stats", DTOutput("summary_table")),  # ✅ Summary Stats
        tabPanel("Metric Over Time", plotlyOutput("metric_time_plot"))  # ✅ Metric Visualization
      )
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
      selectInput("date_col", "Select Date Column:", choices = cols),
      selectInput("metric_col", "Select Available Metrics:", choices = cols, multiple = TRUE)
    )
  })
  
  # Dynamic filtering UI
  output$player_filter <- renderUI({
    req(read_data(), input$player_col)
    selectInput("selected_player", "Filter by Player:",
                choices = unique(read_data()[[input$player_col]]), multiple = TRUE)
  })
  
  output$position_filter <- renderUI({
    req(read_data(), input$position_col)
    selectInput("selected_position", "Filter by Position:",
                choices = unique(read_data()[[input$position_col]]), multiple = TRUE)
  })
  
  output$matchday_filter <- renderUI({
    req(read_data(), input$matchday_col)
    selectInput("selected_matchday", "Filter by Match Day:",
                choices = unique(read_data()[[input$matchday_col]]), multiple = TRUE)
  })
  
  output$task_filter <- renderUI({
    req(read_data(), input$task_col)
    selectInput("selected_task", "Filter by Task:",
                choices = unique(read_data()[[input$task_col]]), multiple = TRUE)
  })
  
  output$date_filter <- renderUI({
    req(read_data(), input$date_col)
    dateRangeInput("selected_date", "Select Date Range:",
                   start = min(as.Date(read_data()[[input$date_col]]), na.rm = TRUE),
                   end = max(as.Date(read_data()[[input$date_col]]), na.rm = TRUE))
  })
  
  # Update metric selection dynamically
  observe({
    req(read_data())
    data <- read_data()
    numeric_cols <- colnames(data)[sapply(data, is.numeric)]
    updateSelectInput(session, "metric", choices = numeric_cols, selected = numeric_cols[1])
  })
  
  # Filtered Data Processing
  processed_data <- reactive({
    req(read_data(), input$player_col, input$position_col, input$matchday_col, input$task_col, input$date_col)
    data <- read_data()
    
    # Apply filters
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
    
    return(data)
  })
  
  # Render Data Table
  output$table <- renderDT({
    req(processed_data())
    datatable(processed_data(), options = list(pageLength = 10))
  })
  
  # Compute Summary Stats
  output$summary_table <- renderDT({
    req(processed_data(), input$metric)
    data <- processed_data()
    
    summary_stats <- data %>%
      summarise(
        Mean = mean(data[[input$metric]], na.rm = TRUE),
        Min = min(data[[input$metric]], na.rm = TRUE),
        Max = max(data[[input$metric]], na.rm = TRUE),
        SD = sd(data[[input$metric]], na.rm = TRUE)
      )
    
    datatable(summary_stats, options = list(pageLength = 5))
  })
  
  # Metric Over Time Plot
  output$metric_time_plot <- renderPlotly({
    req(processed_data(), input$metric, input$date_col)
    
    data <- processed_data()
    
    # Ensure the metric column is numeric
    data[[input$metric]] <- suppressWarnings(as.numeric(data[[input$metric]]))
    
    # Ensure the date column exists
    if (!input$date_col %in% colnames(data)) {
      return(NULL)  # Prevent error if column is missing
    }
    
    # Print date column for debugging
    print("Raw Date Column:")
    print(head(data[[input$date_col]]))
    
    # Remove non-character entries (like factors)
    data[[input$date_col]] <- as.character(data[[input$date_col]])
    
    # Handle date conversion with multiple formats
    data[[input$date_col]] <- case_when(
      grepl("^\\d{4}-\\d{2}-\\d{2}$", data[[input$date_col]]) ~ as.character(ymd(data[[input$date_col]])),
      grepl("^\\d{2}-\\d{2}-\\d{4}$", data[[input$date_col]]) ~ as.character(dmy(data[[input$date_col]])),
      grepl("^\\d{2}/\\d{2}/\\d{4}$", data[[input$date_col]]) ~ as.character(mdy(data[[input$date_col]])),
      TRUE ~ NA_character_
    )
    
    # Convert to Date format
    data[[input$date_col]] <- as.Date(data[[input$date_col]])
    
    # Remove rows with invalid dates
    data <- data[!is.na(data[[input$date_col]]), ]
    
    # Print cleaned dates for debugging
    print("Cleaned Date Column:")
    print(head(data[[input$date_col]]))
    
    # Define color palette
    custom_colors <- c("#3498DB", "#E74C3C", "#2ECC71", "#F1C40F", "#9B59B6")
    
    # Create the column (bar) chart
    p <- ggplot(data, aes(x = !!sym(input$date_col), y = !!sym(input$metric), fill = !!sym(input$metric))) +
      geom_col(width = 0.7, show.legend = FALSE) +  # Column chart
      scale_fill_gradient(low = "#85C1E9", high = "#1F618D") +  # Color gradient
      theme_minimal(base_size = 14) +  # Apply modern theme
      scale_x_date(date_breaks = "1 week", date_labels = "%d-%m-%Y") +  # Format X-axis to dd-mm-yyyy
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        panel.grid.major = element_line(color = "grey80", linetype = "dashed")
      ) +
      labs(
        title = paste(input$metric, "Over Time"),
        x = "Date",
        y = paste("Mean", input$metric)
      )
    
    ggplotly(p)  # Convert to interactive Plotly graph
  })
}

# RUN THE APP
shinyApp(ui, server)