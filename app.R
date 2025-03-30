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
      uiOutput("column_mapping")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DTOutput("table")),
        tabPanel("Summary Stats", DTOutput("summary_table")),
        tabPanel("Barras por Fecha",
                 fluidRow(
                   column(6, uiOutput("filter_players")),
                   column(6, uiOutput("filter_positions"))
                 ),
                 fluidRow(
                   column(6, uiOutput("filter_matchday")),
                   column(6, uiOutput("filter_task"))
                 ),
                 fluidRow(
                   column(6, uiOutput("filter_date_range")),
                   column(6, uiOutput("filter_duration"))
                 ),
                 fluidRow(
                   column(6, selectInput("metric", "Select Metric:", choices = NULL)),
                   column(6, uiOutput("metric_range_filter"))
                 ),
                 plotlyOutput("barras_fecha_plot")
        )
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
      selectInput("duration_col", "Select Duration Column (if available):", choices = c("None", cols)),
      selectInput("start_col", "Select Start Time Column:", choices = c("None", cols)),
      selectInput("end_col", "Select End Time Column:", choices = c("None", cols)),
      selectInput("metric_col", "Select Available Metrics:", choices = cols, multiple = TRUE)
    )
  })
  
  observe({
    req(read_data(), input$metric_col)
    data <- read_data()
    numeric_metrics <- input$metric_col[sapply(data[, input$metric_col, drop = FALSE], is.numeric)]
    updateSelectInput(session, "metric", choices = numeric_metrics)
  })
  
  session_duration <- reactive({
    req(read_data())
    data <- read_data()
    duracion <- NULL
    if (!is.null(input$duration_col) && input$duration_col != "None") {
      duracion <- suppressWarnings(as.numeric(data[[input$duration_col]]))
    } else if (input$start_col != "None" && input$end_col != "None") {
      start <- suppressWarnings(parse_time(data[[input$start_col]]))
      end <- suppressWarnings(parse_time(data[[input$end_col]]))
      duracion <- as.numeric(difftime(end, start, units = "mins"))
    }
    return(duracion)
  })
  
  filtered_data <- reactive({
    req(read_data(), input$player_col, input$position_col, input$matchday_col,
        input$task_col, input$date_col, input$metric)
    data <- read_data()
    if (!is.null(input$filter_selected_players)) {
      data <- data[data[[input$player_col]] %in% input$filter_selected_players, ]
    }
    if (!is.null(input$filter_selected_positions)) {
      data <- data[data[[input$position_col]] %in% input$filter_selected_positions, ]
    }
    if (!is.null(input$filter_selected_matchday)) {
      data <- data[data[[input$matchday_col]] %in% input$filter_selected_matchday, ]
    }
    if (!is.null(input$filter_selected_task)) {
      data <- data[data[[input$task_col]] %in% input$filter_selected_task, ]
    }
    fechas <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y")))
    if (!is.null(input$filter_date_range)) {
      data <- data[fechas >= input$filter_date_range[1] & fechas <= input$filter_date_range[2], ]
    }
    dur <- session_duration()
    if (!is.null(input$filter_duration_range)) {
      keep <- !is.na(dur) & dur >= input$filter_duration_range[1] & dur <= input$filter_duration_range[2]
      data <- data[keep, ]
    }
    if (!is.null(input$metric) && !is.null(input$filter_metric_range)) {
      metric_vals <- suppressWarnings(as.numeric(data[[input$metric]]))
      keep <- !is.na(metric_vals) & metric_vals >= input$filter_metric_range[1] & metric_vals <= input$filter_metric_range[2]
      data <- data[keep, ]
    }
    return(data)
  })
  
  output$filter_players <- renderUI({
    req(read_data(), input$player_col)
    selectInput("filter_selected_players", "Filter by Players:",
                choices = unique(read_data()[[input$player_col]]), multiple = TRUE)
  })
  
  output$filter_positions <- renderUI({
    req(read_data(), input$position_col)
    selectInput("filter_selected_positions", "Filter by Positions:",
                choices = unique(read_data()[[input$position_col]]), multiple = TRUE)
  })
  
  output$filter_matchday <- renderUI({
    req(read_data(), input$matchday_col)
    selectInput("filter_selected_matchday", "Filter by Match Day:",
                choices = unique(read_data()[[input$matchday_col]]), multiple = TRUE)
  })
  
  output$filter_task <- renderUI({
    req(read_data(), input$task_col)
    selectInput("filter_selected_task", "Filter by Task:",
                choices = unique(read_data()[[input$task_col]]), multiple = TRUE)
  })
  
  output$filter_date_range <- renderUI({
    req(read_data(), input$date_col)
    dates <- suppressWarnings(parse_date_time(read_data()[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y")))
    valid_dates <- dates[!is.na(dates)]
    if (length(valid_dates) > 0) {
      dateRangeInput("filter_date_range", "Date Range:",
                     start = min(valid_dates), end = max(valid_dates))
    } else {
      NULL
    }
  })
  
  output$filter_duration <- renderUI({
    req(session_duration())
    dur <- session_duration()
    dur <- dur[!is.na(dur) & is.finite(dur)]
    if (length(dur) > 0) {
      sliderInput("filter_duration_range", "Duration (minutes):",
                  min = floor(min(dur)), max = ceiling(max(dur)),
                  value = c(floor(min(dur)), ceiling(max(dur))))
    } else {
      NULL
    }
  })
  
  output$metric_range_filter <- renderUI({
    req(read_data(), input$metric)
    vals <- suppressWarnings(as.numeric(read_data()[[input$metric]]))
    vals <- vals[!is.na(vals)]
    if (length(vals) > 0) {
      sliderInput("filter_metric_range", paste("Range of", input$metric),
                  min = floor(min(vals)), max = ceiling(max(vals)),
                  value = c(floor(min(vals)), ceiling(max(vals))))
    } else {
      NULL
    }
  })
  
  output$barras_fecha_plot <- renderPlotly({
    req(filtered_data(), input$metric, input$date_col, input$player_col)
    data <- filtered_data()
    data[[input$metric]] <- suppressWarnings(as.numeric(data[[input$metric]]))
    data[[input$date_col]] <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y")))
    plot_data <- data %>%
      group_by(Fecha = .data[[input$date_col]], Jugador = .data[[input$player_col]]) %>%
      summarise(Promedio = mean(.data[[input$metric]], na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(plot_data, aes(x = Fecha, y = Promedio, fill = Jugador)) +
      geom_col(position = "dodge") +
      theme_minimal() +
      labs(title = paste("Promedio de", input$metric, "por Fecha y Jugador"),
           x = "Fecha", y = input$metric) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$table <- renderDT({
    req(filtered_data())
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  output$summary_table <- renderDT({
    req(filtered_data(), input$metric)
    data <- filtered_data()
    vals <- suppressWarnings(as.numeric(data[[input$metric]]))
    stats <- data.frame(
      Mean = mean(vals, na.rm = TRUE),
      Min = min(vals, na.rm = TRUE),
      Max = max(vals, na.rm = TRUE),
      SD = sd(vals, na.rm = TRUE)
    )
    datatable(stats)
  })
}

shinyApp(ui, server)

