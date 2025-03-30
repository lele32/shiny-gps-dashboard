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
library(shiny)
library(bslib)
library(shinythemes)
library(rsconnect)


library(rsconnect)
rsconnect::deployApp('/Users/leandrocarbone/Google Drive/GPS Dashboard/GPS_Shiny')



options(shiny.maxRequestSize = 500 * 1024^2)

# Tema moderno con fuente clara y colores suaves
my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_google("Open Sans"),
  heading_font = font_google("Roboto Slab")
)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Roboto', sans-serif;
      }
      h2 {
        font-family: 'Montserrat', sans-serif;
        font-weight: 600;
      }
      .filter-row {
        display: flex;
        flex-wrap: wrap;
        gap: 20px;
        margin-bottom: 20px;
      }
      .filter-column {
        flex: 1 1 45%;
      }
    "))
  ),
  titlePanel("📊 GPS Data Dashboard"),
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
        tabPanel("📅 Barras por Fecha",
                 tags$div(class = "filter-row",
                          tags$div(class = "filter-column", uiOutput("filtro_jugador")),
                          tags$div(class = "filter-column", uiOutput("filtro_puesto")),
                          tags$div(class = "filter-column", uiOutput("filtro_matchday")),
                          tags$div(class = "filter-column", uiOutput("filtro_tarea")),
                          tags$div(class = "filter-column", uiOutput("filtro_fecha")),
                          tags$div(class = "filter-column", uiOutput("filtro_duracion")),
                          tags$div(class = "filter-column", selectInput("metric", "Select Metric:", choices = NULL)),
                          tags$div(class = "filter-column", uiOutput("filtro_metrica_valor"))
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
      selectInput("duration_col", "Select Duration Column (if available):", choices = c("None", cols), selected = "None"),
      selectInput("start_col", "Select Start Time Column:", choices = c("None", cols), selected = "None"),
      selectInput("end_col", "Select End Time Column:", choices = c("None", cols), selected = "None"),
      selectInput("metric_col", "Select Available Metrics:", choices = cols, multiple = TRUE)
    )
  })
  
  observe({
    req(read_data(), input$metric_col)
    data <- read_data()
    selected_metrics <- input$metric_col
    numeric_metrics <- selected_metrics[sapply(data[selected_metrics], is.numeric)]
    updateSelectInput(session, "metric", choices = numeric_metrics, selected = numeric_metrics[1])
  })
  
  filtro_data <- reactive({
    req(read_data(), input$player_col, input$position_col, input$matchday_col,
        input$task_col, input$date_col)
    data <- read_data()
    
    if (!is.null(input$filtro_jugador)) {
      data <- data[data[[input$player_col]] %in% input$filtro_jugador, ]
    }
    if (!is.null(input$filtro_puesto)) {
      data <- data[data[[input$position_col]] %in% input$filtro_puesto, ]
    }
    if (!is.null(input$filtro_matchday)) {
      data <- data[data[[input$matchday_col]] %in% input$filtro_matchday, ]
    }
    if (!is.null(input$filtro_tarea)) {
      data <- data[data[[input$task_col]] %in% input$filtro_tarea, ]
    }
    if (!is.null(input$filtro_fecha)) {
      fechas <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y")))
      data <- data[fechas >= input$filtro_fecha[1] & fechas <= input$filtro_fecha[2], ]
    }
    
    if (!is.null(input$filtro_duracion_input)) {
      dur <- NULL
      if (!is.null(input$duration_col) && input$duration_col != "None") {
        dur <- suppressWarnings(as.numeric(data[[input$duration_col]]))
      } else if (!is.null(input$start_col) && input$start_col != "None" &&
                 !is.null(input$end_col) && input$end_col != "None") {
        hora_inicio <- suppressWarnings(parse_time(data[[input$start_col]]))
        hora_fin <- suppressWarnings(parse_time(data[[input$end_col]]))
        dur <- as.numeric(difftime(hora_fin, hora_inicio, units = "mins"))
      }
      if (!is.null(dur)) {
        keep <- !is.na(dur) & dur >= input$filtro_duracion_input[1] & dur <= input$filtro_duracion_input[2]
        data <- data[keep, ]
      }
    }
    
    if (!is.null(input$filtro_metrica_valor) && !is.null(input$metric)) {
      metric_vals <- suppressWarnings(as.numeric(data[[input$metric]]))
      keep <- !is.na(metric_vals) & metric_vals >= input$filtro_metrica_valor[1] & metric_vals <= input$filtro_metrica_valor[2]
      data <- data[keep, ]
    }
    
    return(data)
  })
  
  output$filtro_jugador <- renderUI({
    req(read_data(), input$player_col)
    selectInput("filtro_jugador", "Filter by Player:",
                choices = unique(read_data()[[input$player_col]]), multiple = TRUE)
  })
  
  output$filtro_puesto <- renderUI({
    req(read_data(), input$position_col)
    selectInput("filtro_puesto", "Filter by Position:",
                choices = unique(read_data()[[input$position_col]]), multiple = TRUE)
  })
  
  output$filtro_matchday <- renderUI({
    req(read_data(), input$matchday_col)
    selectInput("filtro_matchday", "Filter by Match Day:",
                choices = unique(read_data()[[input$matchday_col]]), multiple = TRUE)
  })
  
  output$filtro_tarea <- renderUI({
    req(read_data(), input$task_col)
    selectInput("filtro_tarea", "Filter by Task:",
                choices = unique(read_data()[[input$task_col]]), multiple = TRUE)
  })
  
  output$filtro_fecha <- renderUI({
    req(read_data(), input$date_col)
    fechas <- suppressWarnings(parse_date_time(read_data()[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y")))
    fechas <- fechas[!is.na(fechas)]
    if (length(fechas) > 0) {
      dateRangeInput("filtro_fecha", "Date Range:",
                     start = min(fechas), end = max(fechas))
    } else {
      return(NULL)
    }
  })
  
  output$filtro_duracion <- renderUI({
    req(read_data())
    data <- read_data()
    dur <- NULL
    
    if (!is.null(input$duration_col) && input$duration_col != "None" && input$duration_col %in% colnames(data)) {
      dur <- suppressWarnings(as.numeric(data[[input$duration_col]]))
    } else if (!is.null(input$start_col) && input$start_col != "None" &&
               !is.null(input$end_col) && input$end_col != "None" &&
               input$start_col %in% colnames(data) && input$end_col %in% colnames(data)) {
      hora_inicio <- suppressWarnings(parse_time(data[[input$start_col]]))
      hora_fin <- suppressWarnings(parse_time(data[[input$end_col]]))
      dur <- as.numeric(difftime(hora_fin, hora_inicio, units = "mins"))
    }
    
    dur <- dur[!is.na(dur) & is.finite(dur) & dur >= 0]  # Evita negativos
    
    if (length(dur) == 0 || all(is.na(dur))) {
      return(NULL)
    }
    
    sliderInput("filtro_duracion_input", "Duration (min):",
                min = floor(min(dur, na.rm = TRUE)),
                max = ceiling(max(dur, na.rm = TRUE)),
                value = c(floor(min(dur, na.rm = TRUE)), ceiling(max(dur, na.rm = TRUE))))
  })
  
  output$filtro_metrica_valor <- renderUI({
    req(read_data(), input$metric)
    data <- read_data()
    values <- suppressWarnings(as.numeric(data[[input$metric]]))
    values <- values[!is.na(values) & is.finite(values)]
    if (length(values) == 0) return(NULL)
    sliderInput("filtro_metrica_valor", paste("Filter", input$metric),
                min = floor(min(values)), max = ceiling(max(values)),
                value = c(floor(min(values)), ceiling(max(values))))
  })
  
  output$table <- renderDT({
    req(filtro_data())
    datatable(filtro_data(), options = list(pageLength = 10))
  })
  
  output$summary_table <- renderDT({
    req(filtro_data(), input$metric)
    data <- filtro_data()
    metric_vals <- suppressWarnings(as.numeric(data[[input$metric]]))
    summary_stats <- data.frame(
      Mean = mean(metric_vals, na.rm = TRUE),
      Min = min(metric_vals, na.rm = TRUE),
      Max = max(metric_vals, na.rm = TRUE),
      SD = sd(metric_vals, na.rm = TRUE)
    )
    datatable(summary_stats)
  })
  
  output$barras_fecha_plot <- renderPlotly({
    req(filtro_data(), input$metric, input$player_col, input$date_col)
    data <- filtro_data()
    data[[input$metric]] <- suppressWarnings(as.numeric(data[[input$metric]]))
    data[[input$date_col]] <- parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
    data <- data[!is.na(data[[input$metric]]) & !is.na(data[[input$date_col]]), ]
    
    # Agrupamos por fecha y jugador
    plot_data <- data %>%
      group_by(Fecha = as.Date(.data[[input$date_col]]), Jugador = .data[[input$player_col]]) %>%
      summarise(Promedio = mean(.data[[input$metric]], na.rm = TRUE), .groups = "drop") %>%
      mutate(Fecha = factor(Fecha, levels = sort(unique(Fecha))))  # Eje discreto
    
    # Gráfico
    p <- ggplot(plot_data, aes(x = Fecha, y = Promedio, fill = Jugador)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      theme_minimal(base_size = 14) +
      labs(title = paste("Promedio de", input$metric, "por Fecha y Jugador"),
           x = "Fecha", y = input$metric) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
}

shinyApp(ui, server)
