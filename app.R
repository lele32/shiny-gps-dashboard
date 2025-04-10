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
library(slider)

options(shiny.maxRequestSize = 500 * 1024^2)

# Tema moderno
tema_gps <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_google("Open Sans"),
  heading_font = font_google("Roboto Slab")
)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML(".filter-row { display: flex; flex-wrap: wrap; gap: 20px; margin-bottom: 20px; }
                    .filter-column { flex: 1 1 45%; }"))
  ),
  tags$div(
    style = "text-align: center; padding: 10px;",
    tags$img(src = "logo.png", height = "80px", style = "margin-bottom: 20px;"),
    tags$h2("📊 GPS Data Dashboard", style = "margin-top: 10px;")
  ),
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
        tabPanel("📊 Métrica en el tiempo",
                 tags$div(class = "filter-row",
                          lapply(c("jugador", "puesto", "matchday", "tarea", "fecha", "duracion"), function(id) {
                            tags$div(class = "filter-column", uiOutput(paste0("filtro_", id)))
                          }),
                          tags$div(class = "filter-column", selectInput("metric", "Select Metric:", choices = NULL)),
                          tags$div(class = "filter-column", uiOutput("filtro_metrica_valor"))
                 ),
                 plotlyOutput("barras_fecha_plot")
        ),
        tabPanel("📦 Boxplot por Match Day",
                 tags$div(class = "filter-row",
                          lapply(c("jugador_box", "puesto_box", "matchday_box", "tarea_box", "fecha_box", "duracion_box"), function(id) {
                            tags$div(class = "filter-column", uiOutput(paste0("filtro_", id)))
                          }),
                          tags$div(class = "filter-column", selectInput("metric_box", "Select Metric:", choices = NULL)),
                          tags$div(class = "filter-column", uiOutput("filtro_metrica_valor_box"))
                 ),
                 plotlyOutput("boxplot_matchday")
        ),
        tabPanel("📦 Boxplot por Tarea",
                 tags$div(class = "filter-row",
                          lapply(c("jugador_task", "puesto_task", "matchday_task", "tarea_task", "fecha_task", "duracion_task"), function(id) {
                            tags$div(class = "filter-column", uiOutput(paste0("filtro_", id)))
                          }),
                          tags$div(class = "filter-column", selectInput("metric_task", "Select Metric:", choices = NULL)),
                          tags$div(class = "filter-column", uiOutput("filtro_metrica_valor_task"))
                 ),
                 plotlyOutput("boxplot_task")
        ),
        tabPanel("📈 Z-score por Fecha",
                 tags$div(class = "filter-row",
                          lapply(c("jugador_z", "puesto_z", "matchday_z", "tarea_z", "fecha_z", "duracion_z"), function(id) {
                            tags$div(class = "filter-column", uiOutput(paste0("filtro_", id)))
                          }),
                          tags$div(class = "filter-column", selectInput("metric_z", "Select Metric:", choices = NULL)),
                          tags$div(class = "filter-column", uiOutput("filtro_metrica_valor_z"))
                 ),
                 plotlyOutput("zscore_plot")
        )
      )
    )
  )
)

##SERVER
server <- function(input, output, session) {
  
  read_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    file_path <- input$file$datapath
    
    data <- switch(ext,
                   "csv" = {
                     first_line <- readLines(file_path, n = 1)
                     delim <- if (grepl(";", first_line)) ";" else ","
                     read_delim(file_path, delim = delim, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
                   },
                   "xlsx" = read_excel(file_path),
                   "json" = fromJSON(file_path, flatten = TRUE),
                   stop("Unsupported file type."))
    colnames(data) <- make.names(colnames(data))
    return(data)
  })
  
  
  # UI para información del archivo
  output$file_info <- renderUI({
    req(input$file)
    tags$p(
      tags$b("File:"), input$file$name, " - ", round(input$file$size / 1024, 2), "KB"
    )
  })
  
  # UI para asignación de columnas clave
  output$column_mapping <- renderUI({
    req(read_data())
    cols <- colnames(read_data())
    
    mapping_inputs <- function(id, label, multiple = FALSE, include_none = FALSE) {
      choices <- if (include_none) c("None", cols) else cols
      selectInput(id, label, choices = choices, multiple = multiple, selected = NULL)
    }
    
    tagList(
      mapping_inputs("player_col", "Select Player Column:"),
      mapping_inputs("position_col", "Select Position Column:"),
      mapping_inputs("matchday_col", "Select Match Day Column:"),
      mapping_inputs("task_col", "Select Task Column:"),
      mapping_inputs("date_col", "Select Date Column:"),
      mapping_inputs("duration_col", "Select Duration Column (if available):", include_none = TRUE),
      mapping_inputs("start_col", "Select Start Time Column:", include_none = TRUE),
      mapping_inputs("end_col", "Select End Time Column:", include_none = TRUE),
      mapping_inputs("metric_col", "Select Available Metrics:", multiple = TRUE)
    )
  })
  
  # Actualización de inputs de métricas válidas
  observe({
    req(read_data(), input$metric_col)
    data <- read_data()
    selected_metrics <- input$metric_col
    valid_metrics <- selected_metrics[selected_metrics %in% colnames(data)]
    numeric_metrics <- valid_metrics[sapply(data[valid_metrics], is.numeric)]
    
    update_inputs <- function(id) {
      updateSelectInput(session, id, choices = numeric_metrics, selected = numeric_metrics[1])
    }
    
    if (length(numeric_metrics) > 0) {
      lapply(c("metric", "metric_box", "metric_task", "metric_z"), update_inputs)
    } else {
      lapply(c("metric", "metric_box", "metric_task", "metric_z"), function(id) {
        updateSelectInput(session, id, choices = character(0))
      })
    }
  })
  
  # Helpers dinámicos para filtros
  create_filter_ui <- function(id, colname, label) {
    renderUI({
      req(read_data())
      data <- read_data()
      if (!is.null(input[[colname]]) && input[[colname]] %in% colnames(data)) {
        selectInput(id, label, choices = unique(data[[input[[colname]]]]), multiple = TRUE)
      }
    })
  }
  
  create_date_filter <- function(id, colname) {
    renderUI({
      req(read_data())
      data <- read_data()
      if (!is.null(input[[colname]]) && input[[colname]] %in% colnames(data)) {
        fechas <- suppressWarnings(parse_date_time(data[[input[[colname]]]], orders = c("Y-m-d", "d-m-Y", "m/d/Y")))
        fechas <- fechas[!is.na(fechas)]
        if (length(fechas) > 0) {
          dateRangeInput(id, "Date Range:", start = min(fechas), end = max(fechas))
        }
      }
    })
  }
  
  create_duration_filter <- function(id) {
    renderUI({
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
      dur <- dur[!is.na(dur) & is.finite(dur) & dur >= 0]
      if (length(dur) > 0) {
        sliderInput(id, "Duration (min):", min = floor(min(dur)), max = ceiling(max(dur)), value = c(floor(min(dur)), ceiling(max(dur))))
      }
    })
  }
  # Filtros por gráfico asignados condicionalmente en observeEvent
  observe({
    output$filtro_jugador       <- create_filter_ui("filtro_jugador", "player_col", "Filter by Player:")
    output$filtro_jugador_box   <- create_filter_ui("filtro_jugador_box", "player_col", "Filter by Player:")
    output$filtro_jugador_task  <- create_filter_ui("filtro_jugador_task", "player_col", "Filter by Player:")
    output$filtro_jugador_z     <- create_filter_ui("filtro_jugador_z", "player_col", "Filter by Player:")
    
    output$filtro_puesto       <- create_filter_ui("filtro_puesto", "position_col", "Filter by Position:")
    output$filtro_puesto_box   <- create_filter_ui("filtro_puesto_box", "position_col", "Filter by Position:")
    output$filtro_puesto_task  <- create_filter_ui("filtro_puesto_task", "position_col", "Filter by Position:")
    output$filtro_puesto_z     <- create_filter_ui("filtro_puesto_z", "position_col", "Filter by Position:")
    
    output$filtro_matchday       <- create_filter_ui("filtro_matchday", "matchday_col", "Filter by Match Day:")
    output$filtro_matchday_box   <- create_filter_ui("filtro_matchday_box", "matchday_col", "Filter by Match Day:")
    output$filtro_matchday_task  <- create_filter_ui("filtro_matchday_task", "matchday_col", "Filter by Match Day:")
    output$filtro_matchday_z     <- create_filter_ui("filtro_matchday_z", "matchday_col", "Filter by Match Day:")
    
    output$filtro_tarea       <- create_filter_ui("filtro_tarea", "task_col", "Filter by Task:")
    output$filtro_tarea_box   <- create_filter_ui("filtro_tarea_box", "task_col", "Filter by Task:")
    output$filtro_tarea_task  <- create_filter_ui("filtro_tarea_task", "task_col", "Filter by Task:")
    output$filtro_tarea_z     <- create_filter_ui("filtro_tarea_z", "task_col", "Filter by Task:")
    
    output$filtro_fecha       <- create_date_filter("filtro_fecha", "date_col")
    output$filtro_fecha_box   <- create_date_filter("filtro_fecha_box", "date_col")
    output$filtro_fecha_task  <- create_date_filter("filtro_fecha_task", "date_col")
    output$filtro_fecha_z     <- create_date_filter("filtro_fecha_z", "date_col")
    
    output$filtro_duracion       <- create_duration_filter("filtro_duracion_input")
    output$filtro_duracion_box   <- create_duration_filter("filtro_duracion_input_box")
    output$filtro_duracion_task  <- create_duration_filter("filtro_duracion_input_task")
    output$filtro_duracion_z     <- create_duration_filter("filtro_duracion_input_z")
  })
  
  output$filtro_metrica_valor <- renderUI({
    req(read_data())
    if (is.null(input$metric) || !(input$metric %in% colnames(read_data()))) return(NULL)
    values <- suppressWarnings(as.numeric(read_data()[[input$metric]]))
    values <- values[!is.na(values) & is.finite(values)]
    if (length(values) == 0) return(NULL)
    sliderInput("filtro_metrica_valor", paste("Filter", input$metric),
                min = floor(min(values)), max = ceiling(max(values)),
                value = c(floor(min(values)), ceiling(max(values))))
  })
  
  output$filtro_metrica_valor_box <- renderUI({
    req(read_data())
    if (is.null(input$metric_box) || !(input$metric_box %in% colnames(read_data()))) return(NULL)
    values <- suppressWarnings(as.numeric(read_data()[[input$metric_box]]))
    values <- values[!is.na(values) & is.finite(values)]
    if (length(values) == 0) return(NULL)
    sliderInput("filtro_metrica_valor_box", paste("Filter", input$metric_box),
                min = floor(min(values)), max = ceiling(max(values)),
                value = c(floor(min(values)), ceiling(max(values))))
  })
  
  output$filtro_metrica_valor_task <- renderUI({
    req(read_data())
    if (is.null(input$metric_task) || !(input$metric_task %in% colnames(read_data()))) return(NULL)
    values <- suppressWarnings(as.numeric(read_data()[[input$metric_task]]))
    values <- values[!is.na(values) & is.finite(values)]
    if (length(values) == 0) return(NULL)
    sliderInput("filtro_metrica_valor_task", paste("Filter", input$metric_task),
                min = floor(min(values)), max = ceiling(max(values)),
                value = c(floor(min(values)), ceiling(max(values))))
  })
  
  output$filtro_metrica_valor_z <- renderUI({
    req(read_data())
    if (is.null(input$metric_z) || !(input$metric_z %in% colnames(read_data()))) return(NULL)
    values <- suppressWarnings(as.numeric(read_data()[[input$metric_z]]))
    values <- values[!is.na(values) & is.finite(values)]
    if (length(values) == 0) return(NULL)
    sliderInput("filtro_metrica_valor_z", paste("Filter", input$metric_z),
                min = floor(min(values)), max = ceiling(max(values)),
                value = c(floor(min(values)), ceiling(max(values))))
  })
  
  filtro_data <- reactive({
    req(read_data())
    data <- read_data()
    
    # Filtro por jugador
    if (!is.null(input$player_col) && input$player_col %in% colnames(data) &&
        !is.null(input$filtro_jugador)) {
      data <- data[data[[input$player_col]] %in% input$filtro_jugador, ]
    }
    
    # Filtro por puesto
    if (!is.null(input$position_col) && input$position_col %in% colnames(data) &&
        !is.null(input$filtro_puesto)) {
      data <- data[data[[input$position_col]] %in% input$filtro_puesto, ]
    }
    
    # Filtro por match day
    if (!is.null(input$matchday_col) && input$matchday_col %in% colnames(data) &&
        !is.null(input$filtro_matchday)) {
      data <- data[data[[input$matchday_col]] %in% input$filtro_matchday, ]
    }
    
    # Filtro por tarea
    if (!is.null(input$task_col) && input$task_col %in% colnames(data) &&
        !is.null(input$filtro_tarea)) {
      data <- data[data[[input$task_col]] %in% input$filtro_tarea, ]
    }
    
    # Filtro por fecha
    if (!is.null(input$date_col) && input$date_col %in% colnames(data) &&
        !is.null(input$filtro_fecha)) {
      fechas <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y")))
      data <- data[!is.na(fechas) & fechas >= input$filtro_fecha[1] & fechas <= input$filtro_fecha[2], ]
    }
    
    # Filtro por duración
    if (!is.null(input$filtro_duracion_input)) {
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
      if (!is.null(dur)) {
        keep <- !is.na(dur) & dur >= input$filtro_duracion_input[1] & dur <= input$filtro_duracion_input[2]
        data <- data[keep, ]
      }
    }
    
    # Filtro por valor de la métrica
    if (!is.null(input$metric) && input$metric %in% colnames(data) &&
        !is.null(input$filtro_metrica_valor)) {
      metric_vals <- suppressWarnings(as.numeric(data[[input$metric]]))
      data <- data[!is.na(metric_vals) & metric_vals >= input$filtro_metrica_valor[1] & metric_vals <= input$filtro_metrica_valor[2], ]
    }
    
    return(data)
  })
  
  filtro_data_box <- reactive({
    req(read_data())
    data <- read_data()
    
    if (!is.null(input$player_col) && input$player_col %in% colnames(data) &&
        !is.null(input$filtro_jugador_box)) {
      data <- data[data[[input$player_col]] %in% input$filtro_jugador_box, ]
    }
    
    if (!is.null(input$position_col) && input$position_col %in% colnames(data) &&
        !is.null(input$filtro_puesto_box)) {
      data <- data[data[[input$position_col]] %in% input$filtro_puesto_box, ]
    }
    
    if (!is.null(input$matchday_col) && input$matchday_col %in% colnames(data) &&
        !is.null(input$filtro_matchday_box)) {
      data <- data[data[[input$matchday_col]] %in% input$filtro_matchday_box, ]
    }
    
    if (!is.null(input$task_col) && input$task_col %in% colnames(data) &&
        !is.null(input$filtro_tarea_box)) {
      data <- data[data[[input$task_col]] %in% input$filtro_tarea_box, ]
    }
    
    if (!is.null(input$date_col) && input$date_col %in% colnames(data) &&
        !is.null(input$filtro_fecha_box)) {
      fechas <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y")))
      data <- data[!is.na(fechas) & fechas >= input$filtro_fecha_box[1] & fechas <= input$filtro_fecha_box[2], ]
    }
    
    if (!is.null(input$filtro_duracion_input_box)) {
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
      if (!is.null(dur)) {
        keep <- !is.na(dur) & dur >= input$filtro_duracion_input_box[1] & dur <= input$filtro_duracion_input_box[2]
        data <- data[keep, ]
      }
    }
    
    if (!is.null(input$metric_box) && input$metric_box %in% colnames(data) &&
        !is.null(input$filtro_metrica_valor_box)) {
      metric_vals <- suppressWarnings(as.numeric(data[[input$metric_box]]))
      data <- data[!is.na(metric_vals) & metric_vals >= input$filtro_metrica_valor_box[1] & metric_vals <= input$filtro_metrica_valor_box[2], ]
    }
    
    return(data)
  })
  
  filtro_data_task <- reactive({
    req(read_data())
    data <- read_data()
    
    if (!is.null(input$player_col) && input$player_col %in% colnames(data) &&
        !is.null(input$filtro_jugador_task)) {
      data <- data[data[[input$player_col]] %in% input$filtro_jugador_task, ]
    }
    
    if (!is.null(input$position_col) && input$position_col %in% colnames(data) &&
        !is.null(input$filtro_puesto_task)) {
      data <- data[data[[input$position_col]] %in% input$filtro_puesto_task, ]
    }
    
    if (!is.null(input$matchday_col) && input$matchday_col %in% colnames(data) &&
        !is.null(input$filtro_matchday_task)) {
      data <- data[data[[input$matchday_col]] %in% input$filtro_matchday_task, ]
    }
    
    if (!is.null(input$task_col) && input$task_col %in% colnames(data) &&
        !is.null(input$filtro_tarea_task)) {
      data <- data[data[[input$task_col]] %in% input$filtro_tarea_task, ]
    }
    
    if (!is.null(input$date_col) && input$date_col %in% colnames(data) &&
        !is.null(input$filtro_fecha_task)) {
      fechas <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y")))
      data <- data[!is.na(fechas) & fechas >= input$filtro_fecha_task[1] & fechas <= input$filtro_fecha_task[2], ]
    }
    
    if (!is.null(input$filtro_duracion_input_task)) {
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
      if (!is.null(dur)) {
        keep <- !is.na(dur) & dur >= input$filtro_duracion_input_task[1] & dur <= input$filtro_duracion_input_task[2]
        data <- data[keep, ]
      }
    }
    
    if (!is.null(input$metric_task) && input$metric_task %in% colnames(data) &&
        !is.null(input$filtro_metrica_valor_task)) {
      metric_vals <- suppressWarnings(as.numeric(data[[input$metric_task]]))
      data <- data[!is.na(metric_vals) & metric_vals >= input$filtro_metrica_valor_task[1] & metric_vals <= input$filtro_metrica_valor_task[2], ]
    }
    
    return(data)
  })
  
  # Reactive filtrado para el gráfico de Z-score
  filtro_data_z <- reactive({
    req(read_data())
    data <- read_data()
    
    if (!is.null(input$player_col) && input$player_col %in% colnames(data) &&
        !is.null(input$filtro_jugador_z)) {
      data <- data[data[[input$player_col]] %in% input$filtro_jugador_z, ]
    }
    
    if (!is.null(input$position_col) && input$position_col %in% colnames(data) &&
        !is.null(input$filtro_puesto_z)) {
      data <- data[data[[input$position_col]] %in% input$filtro_puesto_z, ]
    }
    
    if (!is.null(input$matchday_col) && input$matchday_col %in% colnames(data) &&
        !is.null(input$filtro_matchday_z)) {
      data <- data[data[[input$matchday_col]] %in% input$filtro_matchday_z, ]
    }
    
    if (!is.null(input$task_col) && input$task_col %in% colnames(data) &&
        !is.null(input$filtro_tarea_z)) {
      data <- data[data[[input$task_col]] %in% input$filtro_tarea_z, ]
    }
    
    if (!is.null(input$date_col) && input$date_col %in% colnames(data) &&
        !is.null(input$filtro_fecha_z)) {
      fechas <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y")))
      data <- data[!is.na(fechas) & fechas >= input$filtro_fecha_z[1] & fechas <= input$filtro_fecha_z[2], ]
    }
    
    if (!is.null(input$filtro_duracion_input_z)) {
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
      if (!is.null(dur)) {
        keep <- !is.na(dur) & dur >= input$filtro_duracion_input_z[1] & dur <= input$filtro_duracion_input_z[2]
        data <- data[keep, ]
      }
    }
    
    if (!is.null(input$metric_z) && input$metric_z %in% colnames(data) &&
        !is.null(input$filtro_metrica_valor_z)) {
      metric_vals <- suppressWarnings(as.numeric(data[[input$metric_z]]))
      data <- data[!is.na(metric_vals) & metric_vals >= input$filtro_metrica_valor_z[1] & metric_vals <= input$filtro_metrica_valor_z[2], ]
    }
    
    return(data)
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
  # grafico barras por fecha
  output$barras_fecha_plot <- renderPlotly({
    req(filtro_data(), input$metric)
    data <- filtro_data()
    
    data[[input$metric]] <- suppressWarnings(as.numeric(data[[input$metric]]))
    data[[input$date_col]] <- parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
    data <- data[!is.na(data[[input$metric]]) & !is.na(data[[input$date_col]]), ]
    
    plot_data <- data %>%
      group_by(Fecha = as.Date(.data[[input$date_col]]), Jugador = .data[[input$player_col]]) %>%
      summarise(Promedio = mean(.data[[input$metric]], na.rm = TRUE), .groups = "drop") %>%
      mutate(
        Fecha = factor(Fecha, levels = sort(unique(Fecha))),
        tooltip = paste0("Jugador: ", Jugador, "<br>Fecha: ", Fecha, "<br>Promedio: ", round(Promedio, 2))
      )
    
    p <- ggplot(plot_data, aes(x = Fecha, y = Promedio, fill = Jugador, text = tooltip)) +
      geom_col(position = position_dodge2(preserve = "single"), width = 0.7) +
      scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)]) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Promedio de", input$metric, "por Fecha y Jugador"),
        x = "Fecha", y = input$metric
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(face = "bold", size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        legend.position = "right"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # grafico boxplot por MD
  output$boxplot_matchday <- renderPlotly({
    req(filtro_data_box(), input$metric_box)
    data <- filtro_data_box()
    
    data[[input$metric_box]] <- suppressWarnings(as.numeric(data[[input$metric_box]]))
    data <- data[!is.na(data[[input$metric_box]]), ]
    
    plot_data <- data %>%
      mutate(
        MatchDay = as.factor(.data[[input$matchday_col]]),
        Jugador = .data[[input$player_col]],
        Valor = .data[[input$metric_box]],
        tooltip = paste0("Jugador: ", Jugador, "<br>Match Day: ", MatchDay, "<br>", input$metric_box, ": ", round(Valor, 2))
      )
    
    p <- ggplot(plot_data, aes(x = MatchDay, y = Valor, fill = MatchDay, text = tooltip)) +
      geom_boxplot(outlier.shape = 21, outlier.size = 1.5, outlier.color = "black", alpha = 0.6) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Distribución de", input$metric_box, "por Match Day"),
        x = "Match Day",
        y = input$metric_box
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # grafico boxplot por Tarea
  output$boxplot_task <- renderPlotly({
    req(filtro_data_task(), input$metric_task, input$task_col)
    data <- filtro_data_task()
    
    data[[input$metric_task]] <- suppressWarnings(as.numeric(data[[input$metric_task]]))
    data <- data[!is.na(data[[input$metric_task]]), ]
    
    plot_data <- data %>%
      mutate(
        Tarea = as.factor(.data[[input$task_col]]),
        Jugador = .data[[input$player_col]],
        Valor = .data[[input$metric_task]],
        tooltip = paste0("Jugador: ", Jugador, "<br>Tarea: ", Tarea, "<br>", input$metric_task, ": ", round(Valor, 2))
      )
    
    p <- ggplot(plot_data, aes(x = Tarea, y = Valor, fill = Tarea, text = tooltip)) +
      geom_boxplot(outlier.shape = 21, outlier.size = 1.5, outlier.color = "black", alpha = 0.6) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Distribución de", input$metric_task, "por Tarea"),
        x = "Tarea",
        y = input$metric_task
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45),
        legend.position = "none"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # Render plot Z-score por jugador con media móvil
  output$zscore_plot <- renderPlotly({
    req(filtro_data_z(), input$metric_z, input$player_col, input$date_col)
    
    data <- filtro_data_z()
    
    # Parsear fechas y métrica
    data[[input$date_col]] <- parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
    data[[input$metric_z]] <- suppressWarnings(as.numeric(data[[input$metric_z]]))
    data <- data[!is.na(data[[input$date_col]]) & !is.na(data[[input$metric_z]]), ]
    
    # Calcular Z-score global por jugador
    z_data <- data %>%
      arrange(.data[[input$player_col]], .data[[input$date_col]]) %>%
      group_by(Jugador = .data[[input$player_col]]) %>%
      mutate(
        Valor = .data[[input$metric_z]],
        media_jugador = mean(Valor, na.rm = TRUE),
        sd_jugador = sd(Valor, na.rm = TRUE),
        z = (Valor - media_jugador) / sd_jugador,
        Fecha = as.Date(.data[[input$date_col]]),
        z_color = case_when(
          z >= 1.5 ~ "Alto",
          z <= -1.5 ~ "Bajo",
          TRUE ~ "Neutral"
        ),
        tooltip = paste0("Jugador: ", Jugador, "<br>Fecha: ", Fecha, "<br>Z-score: ", round(z, 2))
      ) %>%
      ungroup() %>%
      filter(!is.na(z), is.finite(z))
    
    # Jugadores a mostrar
    jugadores_disponibles <- unique(z_data$Jugador)
    jugadores_default <- jugadores_disponibles[1:min(12, length(jugadores_disponibles))]
    jugadores_seleccionados <- if (!is.null(input$filtro_jugador_z)) input$filtro_jugador_z else jugadores_default
    z_data <- z_data %>% filter(Jugador %in% jugadores_seleccionados)
    
    # Paleta de colores
    colores_base <- c("Alto" = "#e74c3c", "Bajo" = "#2ecc71", "Neutral" = "#f1c40f")
    fondo_rojo  <- "#fdecea"
    fondo_verde <- "#eafaf1"
    
    fecha_min <- min(z_data$Fecha, na.rm = TRUE)
    fecha_max <- max(z_data$Fecha, na.rm = TRUE)
    
    # Gráfico
    p <- ggplot(z_data, aes(x = Fecha, y = z, text = tooltip, color = z_color)) +
      annotate("rect", xmin = fecha_min, xmax = fecha_max,
               ymin = 1.5, ymax = Inf, fill = fondo_rojo, alpha = 0.4) +
      annotate("rect", xmin = fecha_min, xmax = fecha_max,
               ymin = -Inf, ymax = -1.5, fill = fondo_verde, alpha = 0.4) +
      geom_smooth(aes(group = Jugador),method = "loess", span = 0.9, se = FALSE, color = "#34495e", linewidth = 0.6)+
      geom_point(size = 1) +
      scale_color_manual(values = colores_base, name = "Z-score") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
      facet_wrap(~Jugador, scales = "free_y", ncol = 4) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Z-score de", input$metric_z, "por jugador"),
        x = "Fecha", y = "Z-score"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45),
        strip.text = element_text(face = "bold", size = 13)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.3, y = -0.2))
  })
}

shinyApp(ui, server)
