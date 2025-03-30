library(shiny)
library(readr)
library(readxl)
library(jsonlite)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(hms)

options(shiny.maxRequestSize = 500 * 1024^2)

ui <- fluidPage(
  titlePanel("GPS Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload GPS Data", accept = c(".csv", ".xlsx", ".json")),
      tags$hr(),
      uiOutput("file_info"),
      uiOutput("column_mapping"),
      uiOutput("player_filter"),
      uiOutput("position_filter"),
      uiOutput("matchday_filter"),
      uiOutput("task_filter"),
      uiOutput("date_filter"),
      uiOutput("duration_filter"),
      selectInput("metric", "Select Metric:", choices = NULL),
      uiOutput("metric_value_filter")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DTOutput("table")),
        tabPanel("Summary Stats", DTOutput("summary_table")),
        tabPanel("Metric Over Time", plotlyOutput("metric_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  read_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    file_path <- input$file$datapath
    data <- switch(ext,
                   "csv" = read_csv(file_path, show_col_types = FALSE),
                   "xlsx" = read_excel(file_path),
                   "json" = fromJSON(file_path, flatten = TRUE),
                   stop("Unsupported file type."))
    colnames(data) <- make.names(colnames(data))
    return(data)
  })
  
  output$file_info <- renderUI({
    req(input$file)
    HTML(paste0("<b>File Name:</b> ", input$file$name, "<br>",
                "<b>File Size:</b> ", round(input$file$size / 1024, 2), " KB"))
  })
  
  output$column_mapping <- renderUI({
    req(read_data())
    cols <- colnames(read_data())
    tagList(
      selectInput("player_col", "Select Player Column:", choices = cols),
      selectInput("position_col", "Select Position Column:", choices = cols),
      selectInput("matchday_col", "Select Match Day Column:", choices = cols),
      selectInput("task_col", "Select Task Column:", choices = cols),
      selectInput("date_col", "Select Date Column:", choices = cols),
      selectInput("duration_col", "Select Duration Column (if available):", choices = c("None", cols), selected = "None"),
      selectInput("start_col", "Select Start Time Column:", choices = c("None", cols), selected = "None"),
      selectInput("end_col", "Select End Time Column:", choices = c("None", cols), selected = "None"),
      selectInput("metric_col", "Select Available Metrics:", choices = cols, multiple = TRUE)
    )
  })
  
  observe({
    req(read_data())
    data <- read_data()
    numeric_cols <- colnames(data)[sapply(data, is.numeric)]
    updateSelectInput(session, "metric", choices = numeric_cols)
  })
  
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
    dates <- suppressWarnings(parse_date_time(read_data()[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y")))
    valid_dates <- dates[!is.na(dates)]
    if (length(valid_dates) == 0) return(NULL)
    dateRangeInput("selected_date", "Select Date Range:",
                   start = min(valid_dates), end = max(valid_dates))
  })
  
  session_duration <- reactive({
    req(read_data())
    data <- read_data()
    duracion <- NULL
    
    if (!is.null(input$duration_col) && input$duration_col != "None") {
      duracion <- suppressWarnings(as.numeric(data[[input$duration_col]]))
    } else if (!is.null(input$start_col) && !is.null(input$end_col) &&
               input$start_col != "None" && input$end_col != "None") {
      hora_inicio <- suppressWarnings(parse_time(data[[input$start_col]]))
      hora_fin <- suppressWarnings(parse_time(data[[input$end_col]]))
      duracion <- as.numeric(difftime(hora_fin, hora_inicio, units = "mins"))
    }
    return(duracion)
  })
  
  output$duration_filter <- renderUI({
    dur <- session_duration()
    dur <- dur[!is.na(dur) & is.finite(dur)]
    if (length(dur) == 0) return(NULL)
    sliderInput("duration_range", "Filter by Duration (minutes):",
                min = floor(min(dur)), max = ceiling(max(dur)),
                value = c(floor(min(dur)), ceiling(max(dur))))
  })
  
  output$metric_value_filter <- renderUI({
    req(read_data(), input$metric)
    data <- read_data()
    values <- suppressWarnings(as.numeric(data[[input$metric]]))
    values <- values[!is.na(values) & is.finite(values)]
    if (length(values) == 0) return(NULL)
    sliderInput("selected_metric_range", paste("Filter", input$metric),
                min = floor(min(values)), max = ceiling(max(values)),
                value = c(floor(min(values)), ceiling(max(values))))
  })
  
  processed_data <- reactive({
    req(read_data(), input$player_col, input$position_col,
        input$matchday_col, input$task_col, input$date_col)
    
    data <- read_data()
    
    if (!is.null(input$selected_position)) {
      data <- data[data[[input$position_col]] %in% input$selected_position, ]
    }
    if (!is.null(input$selected_player)) {
      data <- data[data[[input$player_col]] %in% input$selected_player, ]
    }
    if (!is.null(input$selected_matchday)) {
      data <- data[data[[input$matchday_col]] %in% input$selected_matchday, ]
    }
    if (!is.null(input$selected_task)) {
      data <- data[data[[input$task_col]] %in% input$selected_task, ]
    }
    if (!is.null(input$selected_date)) {
      fechas <- parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
      data <- data[!is.na(fechas) & fechas >= input$selected_date[1] & fechas <= input$selected_date[2], ]
    }
    dur <- session_duration()
    if (!is.null(input$duration_range) && length(dur) == nrow(data)) {
      keep <- !is.na(dur) & dur >= input$duration_range[1] & dur <= input$duration_range[2]
      data <- data[keep, ]
    }
    if (!is.null(input$selected_metric_range) && input$metric %in% colnames(data)) {
      metric_vals <- suppressWarnings(as.numeric(data[[input$metric]]))
      keep <- !is.na(metric_vals) & metric_vals >= input$selected_metric_range[1] & metric_vals <= input$selected_metric_range[2]
      data <- data[keep, ]
    }
    
    return(data)
  })
  
  output$table <- renderDT({
    req(processed_data())
    datatable(processed_data(), options = list(pageLength = 10))
  })
  
  output$summary_table <- renderDT({
    req(processed_data(), input$metric)
    data <- processed_data()
    vals <- suppressWarnings(as.numeric(data[[input$metric]]))
    summary <- data.frame(
      Mean = mean(vals, na.rm = TRUE),
      Min = min(vals, na.rm = TRUE),
      Max = max(vals, na.rm = TRUE),
      SD = sd(vals, na.rm = TRUE)
    )
    datatable(summary)
  })
  
  output$metric_plot <- renderPlotly({
    req(processed_data(), input$metric, input$date_col)
    data <- processed_data()
    data[[input$metric]] <- suppressWarnings(as.numeric(data[[input$metric]]))
    data[[input$date_col]] <- parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
    data <- data[!is.na(data[[input$date_col]]) & !is.na(data[[input$metric]]), ]
    p <- ggplot(data, aes(x = .data[[input$date_col]], y = .data[[input$metric]], fill = .data[[input$metric]])) +
      geom_col(show.legend = FALSE) +
      scale_fill_gradient(low = "#AED6F1", high = "#2E86C1") +
      theme_minimal(base_size = 14) +
      labs(title = paste(input$metric, "Over Time"), x = "Date", y = input$metric) +
      scale_x_datetime(date_labels = "%d-%m-%Y", date_breaks = "1 week") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
}

shinyApp(ui, server)
