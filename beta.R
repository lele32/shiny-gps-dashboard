# =======================================================
# 📦 LIBRARIES
# =======================================================
library(shiny)           # Web application framework
library(readr)           # CSV reading
library(readxl)          # XLSX reading
library(jsonlite)        # JSON reading
library(DT)              # DataTables output
library(ggplot2)         # Plotting engine
library(plotly)          # Interactive plots
library(dplyr)           # Data wrangling
library(lubridate)       # Date handling
library(hms)             # Time handling
library(bslib)           # Bootstrap theming
library(shinythemes)     # Built-in themes
library(rsconnect)       # Deployment
library(slider)          # Rolling window stats
library(shinyWidgets)    # Custom inputs
library(tidyr)           # Data reshaping
library(fontawesome)     # íconos modernos
library(TTR)             # Para usar TTR::EMA


# =======================================================
# ⚙️ OPTIONS
# =======================================================
options(shiny.maxRequestSize = 500 * 1024^2)  # Allow large file uploads

# ──────────────────────────────────────────────
# 🎨 Tema visual personalizado LIFT
# ──────────────────────────────────────────────
tema_gps <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = bslib::font_google("Open Sans"),
  heading_font = bslib::font_google("Righteous"),
  bg = "#1E1E1E",        # Negro LIFT
  fg = "#ffffff",        # Blanco LIFT
  primary = "#fd002b",   # Rojo LIFT
  secondary = "#c8c8c8"  # Gris LIFT
)

# ──────────────────────────────────────────────
# 🧩 Interfaz de usuario (UI)
# ──────────────────────────────────────────────
ui <- fluidPage(
  theme = tema_gps,
  
  # Estilos personalizados y fuentes
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Righteous&family=Open+Sans&display=swap"),
    tags$style(HTML("
    h2 {
      font-family: 'Righteous', cursive;
      color: #fd002b;
      text-transform: uppercase;
      font-size: 36px;
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
    .shiny-input-container {
      font-family: 'Neo Sans Std', sans-serif;
      color: #ffffff;
    }
    .tabbable > .nav-tabs > li > a {
      color: #ffffff;
      background-color: #1E1E1E;
      border-color: #fd002b;
    }
    .tabbable > .nav-tabs > li.active > a {
      background-color: #fd002b;
      color: #ffffff;
    }
    /* Header DataTables */
    table.dataTable thead th {
      background-color: #fd002b !important;
      color: #ffffff !important;
      font-family: 'Righteous', cursive !important;
      font-size: 15px !important;
      font-weight: bold !important;
    }
    /* Cuerpo de la tabla */
    table.dataTable tbody td {
      background-color: #1e1e1e !important;
      color: #ffffff !important;
      font-family: 'Open Sans', sans-serif !important;
      font-size: 14px !important;
    }
    /* Hover en filas */
    table.dataTable tbody tr:hover {
      background-color: #2a2a2a !important;
    }
  "))
  ),
  
  # Encabezado principal
  tags$div(
    style = "text-align: center; padding: 20px;",
    tags$img(src = "logo.png", height = "80px", style = "margin-bottom: 10px;"),
    tags$h2("📊 GPS Data Dashboard")
  ),
  
  # Layout principal con sidebar
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
        
        tabPanel("📊 Métrica en el tiempo",
                 tags$div(class = "filter-row",
                          lapply(c("jugador", "puesto", "matchday", "tarea", "fecha", "duracion"), function(id) {
                            tags$div(class = "filter-column", uiOutput(paste0("filtro_", id)))
                          }),
                          tags$div(class = "filter-column", selectInput("metric", "Select Metric:", choices = NULL, multiple = TRUE))
                 ),
                 uiOutput("barras_fecha_ui")
        ),
        
        tabPanel("📦 Boxplot por Match Day",
                 tags$div(class = "filter-row",
                          lapply(c("jugador_box", "puesto_box", "matchday_box", "tarea_box", "fecha_box", "duracion_box"), function(id) {
                            tags$div(class = "filter-column", uiOutput(paste0("filtro_", id)))
                          }),
                          tags$div(class = "filter-column", selectInput("metric_box", "Select Metrics:", choices = NULL, multiple = TRUE))
                 ),
                 uiOutput("boxplot_matchday_ui")
        ),
        
        tabPanel("📦 Boxplot por Tarea",
                 tags$div(class = "filter-row",
                          lapply(c("jugador_task", "puesto_task", "matchday_task", "tarea_task", "fecha_task", "duracion_task"), function(id) {
                            tags$div(class = "filter-column", uiOutput(paste0("filtro_", id)))
                          }),
                          tags$div(class = "filter-column", selectInput("metric_task", "Select Metrics:", choices = NULL, multiple = TRUE))
                 ),
                 uiOutput("boxplot_task_ui")
        ),
        
        tabPanel("📈 Z-score por Fecha",
                 tags$div(class = "filter-row",
                          lapply(c("jugador_z", "puesto_z", "matchday_z", "tarea_z", "fecha_z", "duracion_z"), function(id) {
                            tags$div(class = "filter-column", uiOutput(paste0("filtro_", id)))
                          }),
                          tags$div(class = "filter-column", selectInput("metric_z", "Select Metric:", choices = NULL, multiple = TRUE))
                 ),
                 uiOutput("zscore_plot_ui")
        ),
        
        tabPanel("🧪 Análisis de sesión",
                 tags$div(class = "filter-row",
                          lapply(c("jugador_sesion", "puesto_sesion", "matchday_sesion", "tarea_sesion", "duracion_sesion"), function(id) {
                            tags$div(class = "filter-column", uiOutput(paste0("filtro_", id)))
                          }),
                          tags$div(class = "filter-column", uiOutput("filtro_sesion_selector")),
                          tags$div(class = "filter-column", selectInput("metricas_sesion_plot", "Select Metrics:", choices = NULL, multiple = TRUE)),
                          tags$div(class = "filter-column", uiOutput("filtro_fecha_sesion"))
                 ),
                 uiOutput("graficos_metricas_sesion")
        ),
        
        tabPanel("📊 Competitive Analysis",
                 tags$div(class = "filter-row",
                          tags$div(class = "filter-column", uiOutput("filtro_jugador_z_comp")),
                          tags$div(class = "filter-column", uiOutput("filtro_puesto_z_comp")),
                          tags$div(class = "filter-column", uiOutput("filtro_tarea_z_comp")),
                          tags$div(class = "filter-column", uiOutput("filtro_sesion_selector_comp")),
                          tags$div(class = "filter-column", uiOutput("filtro_duracion_z_comp")),
                          tags$div(class = "filter-column", selectInput("metric_z_comp", "Select Metrics:", choices = NULL, multiple = TRUE)),
                          tags$div(class = "filter-column", sliderInput("ventana_movil_z_comp", "Tamaño ventana móvil (partidos anteriores):", min = 3, max = 5, value = 3, step = 1))
                 ),
                 uiOutput("zscore_comp_plot_ui"),
                 tags$hr(),
                 DTOutput("tabla_resumen_comp")
          ),
        tabPanel("📈 ACWR",
                 tags$div(class = "filter-row",
                          tags$div(class = "filter-column", uiOutput("filtro_jugador_acwr")),
                          tags$div(class = "filter-column", uiOutput("filtro_puesto_acwr")),
                          tags$div(class = "filter-column", uiOutput("filtro_matchday_acwr")),
                          tags$div(class = "filter-column", uiOutput("filtro_tarea_acwr")),
                          tags$div(class = "filter-column", uiOutput("filtro_fecha_acwr")),
                          tags$div(class = "filter-column", uiOutput("filtro_duracion_acwr")),
                          tags$div(class = "filter-column", selectInput("metric_acwr", "Select Metrics:", choices = NULL, multiple = TRUE)),
                          
                          # 🎯 Nuevo bloque para Agudo / Crónico usando sliders
                          tags$div(class = "filter-column",
                                   sliderInput(
                                     inputId = "acwr_agudo_dias",
                                     label = tags$span(style = "color:#fd002b; font-weight:bold;", "Días Agudo (ACWR)"),
                                     min = 3, max = 14, value = 7, step = 1
                                   )
                          ),
                          tags$div(class = "filter-column",
                                   sliderInput(
                                     inputId = "acwr_cronico_dias",
                                     label = tags$span(style = "color:#fd002b; font-weight:bold;", "Días Crónico (ACWR)"),
                                     min = 14, max = 42, value = 28, step = 1
                                   )
                          )
                 ),
                 uiOutput("acwr_plot_ui")
        ),
      )
    )
  )
)

# =======================================================
# ⚙️ SERVER
# =======================================================
server <- function(input, output, session) {
  
  # REACTIVO PRINCIPAL: CARGA Y FORMATEO DEL ARCHIVO --------------------------
  read_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    file_path <- input$file$datapath
    
    if (ext == "csv") {
      # Detectar delimitador automáticamente (coma o punto y coma)
      first_line <- readLines(file_path, n = 1)
      delim <- if (grepl(";", first_line)) ";" else ","
      
      # Leer todas las líneas del archivo como texto
      raw_lines <- readLines(file_path, warn = FALSE)
      
      # Detectar la primera línea con encabezado válido: "Player Name" o "Username"
      header_line <- which(grepl('^(\"?Player Name\"?|\"?Username\"?)\\s*[,;]', raw_lines))[1]
      
      if (is.na(header_line)) {
        stop("No se pudo detectar una línea de encabezado válida (Player Name o Username).")
      }
      
      # Leer los datos a partir de la línea detectada
      data <- read_delim(
        file = file_path,
        delim = delim,
        skip = header_line - 1,
        locale = locale(encoding = "UTF-8"),
        show_col_types = FALSE
      )
    } else if (ext == "xlsx") {
      data <- read_excel(file_path)
    } else if (ext == "json") {
      data <- fromJSON(file_path, flatten = TRUE)
    } else {
      stop("Unsupported file type.")
    }
    
    colnames(data) <- make.names(colnames(data))
    # Mapeo automático de columnas basado en nombres
    update_mapped_columns <- function(cols) {
      guess_column <- function(possible_names) {
        match <- tolower(cols) %in% tolower(possible_names)
        if (any(match)) return(cols[which(match)[1]]) else return(NULL)
      }
      
      updateSelectInput(session, "player_col", selected = guess_column(c("player", "player name", "username", "jugador")))
      updateSelectInput(session, "position_col", selected = guess_column(c("position", "pos", "rol", "puesto")))
      updateSelectInput(session, "matchday_col", selected = guess_column(c("match day", "matchday","match.day", "md", "dia", "día")))
      updateSelectInput(session, "task_col", selected = guess_column(c("task", "activity", "drill", "selection", "tarea")))
      updateSelectInput(session, "date_col", selected = guess_column(c("date", "fecha", "session date", "day")))
      updateSelectInput(session, "duration_col", selected = guess_column(c("duration", "duración", "duration_min")))
      updateSelectInput(session, "start_col", selected = guess_column(c("start", "inicio", "hora inicio", "start.hour")))
      updateSelectInput(session, "end_col", selected = guess_column(c("end", "fin", "hora fin", "end_time","final.hour")))
      
      # Detectar métricas numéricas candidatas
      numeric_metrics <- cols[sapply(data[cols], is.numeric)]
      updateSelectInput(session, "metric_col", choices = numeric_metrics, selected = character(0))
    }
    
    update_mapped_columns(colnames(data))
    return(data)
  })
  
  # INFORMACIÓN DEL ARCHIVO ----------------------------------------------------
  output$file_info <- renderUI({
    req(input$file)
    tags$p(tags$b("File:"), input$file$name, " - ", round(input$file$size / 1024, 2), "KB")
  })
  
  # MAPEOS DE COLUMNAS CLAVE ---------------------------------------------------
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
  
  #' 🔄 Actualiza inputs de métricas individuales por pestaña
  #'
  #' Este bloque `observe()` se ejecuta cada vez que se cargan nuevos datos (`read_data()`)
  #' o se modifican las métricas seleccionadas globalmente (`input$metric_col`).
  #'
  #' Su objetivo es:
  #' - Validar qué métricas son numéricas y están presentes en los datos cargados.
  #' - Actualizar los inputs individuales por pestaña: `metric`, `metric_box`, `metric_task`, `metric_z`.
  #' - Si no hay métricas válidas, deja esos inputs vacíos.
  #'
  #' Esto asegura que cada tabPanel tenga un selector con métricas numéricas válidas, coherente
  #' con la selección global realizada en el panel lateral de mapeo de columnas.
  
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
      lapply(c("metric", "metric_box", "metric_task", "metric_z", "metric_acwr"), update_inputs)
    } else {
      lapply(c("metric", "metric_box", "metric_task", "metric_z", "metric_acwr"), function(id) {
        updateSelectInput(session, id, choices = character(0))
      })
    }
  })
  
  # =======================================================
  # 📡 REACTIVE OBSERVER PARA METRIC_Z_COMP
  # =======================================================
  # Este bloque se asegura de que el selector de métrica en la pestaña
  # Competitive Analysis ("metric_z_comp") se actualice dinámicamente
  # con las métricas numéricas seleccionadas en "metric_col".
  # Esto soluciona el error donde el gráfico no aparecía por no tener
  # métricas cargadas por default en ese tab.
  observe({
    req(read_data(), input$metric_col)
    data <- read_data()
    selected_metrics <- input$metric_col
    valid_metrics <- selected_metrics[selected_metrics %in% colnames(data)]
    numeric_metrics <- valid_metrics[sapply(data[valid_metrics], is.numeric)]
    
    updateSelectInput(session, "metric_z_comp",
                      choices = numeric_metrics,
                      selected = head(numeric_metrics, 1))
  })
  
  #' 📌 Actualiza métricas disponibles para Análisis de Sesión
  #'
  #' Este bloque `observe()` se encarga de mantener sincronizado el selector de métricas (`metricas_sesion_plot`)
  #' que se utiliza en la pestaña de análisis de sesión puntual. Cada vez que se actualiza el conjunto de
  #' métricas disponibles (`input$metric_col`), este bloque actualiza las opciones del input correspondiente.
  #' También establece la primera métrica como seleccionada por defecto.
  observe({
    req(input$metric_col)
    updateSelectInput(session, "metricas_sesion_plot",
                      choices = input$metric_col,
                      selected = input$metric_col[1])
  })
  
  # Crea un filtro UI selectInput dinámico basado en la columna indicada
  # === UI dinámico para filtros de selectInput categóricos (jugador, puesto, etc.) ===
  create_filter_ui <- function(id, colname, label) {
    renderUI({
      req(read_data())
      data <- read_data()
      if (!is.null(input[[colname]]) && input[[colname]] %in% colnames(data)) {
        selectInput(id, label, choices = unique(data[[input[[colname]]]]), multiple = TRUE)
      }
    })
  }
  # Crea un filtro UI dateRangeInput dinámico basado en la columna de fecha mapeada
  # === UI dinámico para filtros por rango de fechas ===
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
  # Crea un filtro UI sliderInput para duración, usando columna directa o calculada (hora inicio - hora fin)
  # === UI dinámico para filtro de duración (directa o derivada) ===
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
  # Este bloque crea dinámicamente todos los filtros individuales de cada gráfico/tab.
  # Usa los helpers `create_filter_ui()`, `create_date_filter()` y `create_duration_filter()` definidos previamente.
  # Cada output está vinculado a un filtro de UI único para mantener independencia entre los gráficos.
  observe({
    output$filtro_jugador       <- create_filter_ui("filtro_jugador", "player_col", "Filter by Player:")
    output$filtro_jugador_box   <- create_filter_ui("filtro_jugador_box", "player_col", "Filter by Player:")
    output$filtro_jugador_task  <- create_filter_ui("filtro_jugador_task", "player_col", "Filter by Player:")
    output$filtro_jugador_z     <- create_filter_ui("filtro_jugador_z", "player_col", "Filter by Player:")
    output$filtro_jugador_sesion <- create_filter_ui("filtro_jugador_sesion", "player_col", "Filtrar por Jugador")
    output$filtro_jugador_z_comp <- create_filter_ui("filtro_jugador_z_comp", "player_col", "Filter by Player:")
    
    
    output$filtro_puesto       <- create_filter_ui("filtro_puesto", "position_col", "Filter by Position:")
    output$filtro_puesto_box   <- create_filter_ui("filtro_puesto_box", "position_col", "Filter by Position:")
    output$filtro_puesto_task  <- create_filter_ui("filtro_puesto_task", "position_col", "Filter by Position:")
    output$filtro_puesto_z     <- create_filter_ui("filtro_puesto_z", "position_col", "Filter by Position:")
    output$filtro_puesto_sesion  <- create_filter_ui("filtro_puesto_sesion", "position_col", "Filtrar por Puesto")
    output$filtro_puesto_z_comp  <- create_filter_ui("filtro_puesto_z_comp", "position_col", "Filter by Position:")
    
    
    output$filtro_matchday       <- create_filter_ui("filtro_matchday", "matchday_col", "Filter by Match Day:")
    output$filtro_matchday_box   <- create_filter_ui("filtro_matchday_box", "matchday_col", "Filter by Match Day:")
    output$filtro_matchday_task  <- create_filter_ui("filtro_matchday_task", "matchday_col", "Filter by Match Day:")
    output$filtro_matchday_z     <- create_filter_ui("filtro_matchday_z", "matchday_col", "Filter by Match Day:")
    output$filtro_matchday_sesion <- create_filter_ui("filtro_matchday_sesion", "matchday_col", "Filtrar por Match Day")
    
    
    output$filtro_tarea       <- create_filter_ui("filtro_tarea", "task_col", "Filter by Task:")
    output$filtro_tarea_box   <- create_filter_ui("filtro_tarea_box", "task_col", "Filter by Task:")
    output$filtro_tarea_task  <- create_filter_ui("filtro_tarea_task", "task_col", "Filter by Task:")
    output$filtro_tarea_z     <- create_filter_ui("filtro_tarea_z", "task_col", "Filter by Task:")
    output$filtro_tarea_sesion <- create_filter_ui("filtro_tarea_sesion", "task_col", "Filtrar por Tarea")
    output$filtro_tarea_z_comp   <- create_filter_ui("filtro_tarea_z_comp", "task_col", "Filter by Task:")
    
    output$filtro_fecha       <- create_date_filter("filtro_fecha", "date_col")
    output$filtro_fecha_box   <- create_date_filter("filtro_fecha_box", "date_col")
    output$filtro_fecha_task  <- create_date_filter("filtro_fecha_task", "date_col")
    output$filtro_fecha_z     <- create_date_filter("filtro_fecha_z", "date_col")
    output$filtro_fecha_sesion <- create_date_filter("filtro_fecha_sesion", "date_col")
    
    output$filtro_duracion       <- create_duration_filter("filtro_duracion_input")
    output$filtro_duracion_box   <- create_duration_filter("filtro_duracion_input_box")
    output$filtro_duracion_task  <- create_duration_filter("filtro_duracion_input_task")
    output$filtro_duracion_z     <- create_duration_filter("filtro_duracion_input_z")
    output$filtro_duracion_sesion <- create_duration_filter("filtro_duracion_input_sesion")
    output$filtro_duracion_z_comp <- create_duration_filter("filtro_duracion_input_z_comp")
    
    output$filtro_puesto_acwr    <- create_filter_ui("filtro_puesto_acwr", "position_col", "Filter by Position:")
    output$filtro_matchday_acwr  <- create_filter_ui("filtro_matchday_acwr", "matchday_col", "Filter by Match Day:")
    output$filtro_tarea_acwr     <- create_filter_ui("filtro_tarea_acwr", "task_col", "Filter by Task:")
    output$filtro_fecha_acwr     <- create_date_filter("filtro_fecha_acwr", "date_col")
    output$filtro_duracion_acwr  <- create_duration_filter("filtro_duracion_input_acwr")
  })
  
  
  #Filtros aplicados a grafico de Boxplot de Métrica en MD
  output$filtro_metrica_valor_box <- renderUI({
    req(read_data(), input$metric_box)
    sliders <- lapply(input$metric_box, function(metrica) {
      if (!metrica %in% colnames(read_data())) return(NULL)
      
      values <- suppressWarnings(as.numeric(read_data()[[metrica]]))
      values <- values[!is.na(values) & is.finite(values)]
      if (length(values) == 0) return(NULL)
      
      sliderInput(
        inputId = paste0("filtro_metrica_valor_box_", make.names(metrica)),
        label = paste("Filtrar", metrica),
        min = floor(min(values)),
        max = ceiling(max(values)),
        value = c(floor(min(values)), ceiling(max(values)))
      )
    })
    
    do.call(tagList, sliders)
  })
  
  # Filtro para valores de métrica en gráfico por Tarea
  output$filtro_metrica_valor_task <- renderUI({
    req(read_data(), input$metric_task)
    if (!(input$metric_task %in% colnames(read_data()))) return(NULL)
    values <- suppressWarnings(as.numeric(read_data()[[input$metric_task]]))
    values <- values[!is.na(values) & is.finite(values)]
    if (length(values) == 0) return(NULL)
    
    sliderInput("filtro_metrica_valor_task", paste("Filter", input$metric_task),
                min = floor(min(values)), max = ceiling(max(values)),
                value = c(floor(min(values)), ceiling(max(values))))
  })
  
  # Filtro para valores de métrica en gráfico Z-score
  output$filtro_metrica_valor_z <- renderUI({
    req(input$metric_z, read_data())
    
    sliders <- lapply(input$metric_z, function(metrica) {
      metrica_clean <- make.names(metrica)
      if (!(metrica %in% names(read_data()))) return(NULL)
      
      values <- suppressWarnings(as.numeric(read_data()[[metrica]]))
      values <- values[!is.na(values) & is.finite(values)]
      if (length(values) == 0) return(NULL)
      
      sliderInput(
        inputId = paste0("filtro_metrica_valor_z_", metrica_clean),
        label = paste("Filtro de valores para", metrica),
        min = floor(min(values)),
        max = ceiling(max(values)),
        value = c(floor(min(values)), ceiling(max(values)))
      )
    })
    
    do.call(tagList, sliders)
  })
  
  # Filtro para valores de métrica en análisis de sesión
  output$filtro_metrica_valor_sesion <- renderUI({
    req(read_data(), input$metricas_sesion_plot)
    data <- read_data()
    metrica <- input$metricas_sesion_plot[1]
    if (is.null(metrica) || !(metrica %in% names(data))) return(NULL)
    values <- suppressWarnings(as.numeric(data[[metrica]]))
    values <- values[!is.na(values) & is.finite(values)]
    if (length(values) == 0) return(NULL)
    
    sliderInput("filtro_metrica_valor_sesion", paste("Filtrar", metrica),
                min = floor(min(values)), max = ceiling(max(values)),
                value = c(floor(min(values)), ceiling(max(values))))
  })
  
  # Filtro de selección de sesiones específicas (por fecha) para análisis de sesión
  output$filtro_sesion_selector <- renderUI({
    req(read_data(), input$date_col)
    data <- read_data()
    if (!(input$date_col %in% names(data))) return(NULL)
    
    # Aplicar filtros condicionales si existen
    if (!is.null(input$matchday_col) && input$matchday_col %in% names(data) &&
        !is.null(input$filtro_matchday_sesion)) {
      data <- data[data[[input$matchday_col]] %in% input$filtro_matchday_sesion, ]
    }
    if (!is.null(input$task_col) && input$task_col %in% names(data) &&
        !is.null(input$filtro_tarea_sesion)) {
      data <- data[data[[input$task_col]] %in% input$filtro_tarea_sesion, ]
    }
    
    fechas <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y")))
    fechas <- fechas[!is.na(fechas)]
    if (length(fechas) == 0) return(NULL)
    
    pickerInput(
      inputId = "filtro_sesion_selector",
      label = "Seleccionar sesiones específicas",
      choices = sort(unique(as.character(fechas))),
      multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })
  
  # Filtro de selección de sesión (una fecha) para análisis competitivo
  output$filtro_sesion_selector_comp <- renderUI({
    req(read_data(), input$date_col)
    data <- read_data()
    if (!(input$date_col %in% names(data))) return(NULL)
    
    # Filtrar solo MD
    if (!is.null(input$matchday_col) && input$matchday_col %in% names(data)) {
      data <- data[data[[input$matchday_col]] == "MD", ]
    }
    
    # Filtrar por tarea
    if (!is.null(input$task_col) && input$task_col %in% names(data) &&
        !is.null(input$filtro_tarea_z_comp)) {
      data <- data[data[[input$task_col]] %in% input$filtro_tarea_z_comp, ]
    }
    
    fechas <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y")))
    fechas <- fechas[!is.na(fechas)]
    if (length(fechas) == 0) return(NULL)
    
    pickerInput(
      inputId = "filtro_sesion_selector_comp",
      label = "Seleccionar sesión (fecha de partido)",
      choices = sort(unique(as.character(fechas))),
      multiple = FALSE,
      options = list(`live-search` = TRUE)
    )
  })
  
  # Filtro de valores de métrica para análisis competitivo
  output$filtro_metrica_valor_z_comp <- renderUI({
    req(read_data(), input$metric_z_comp)
    if (!(input$metric_z_comp %in% colnames(read_data()))) return(NULL)
    
    values <- suppressWarnings(as.numeric(read_data()[[input$metric_z_comp]]))
    values <- values[!is.na(values) & is.finite(values)]
    if (length(values) == 0) return(NULL)
    
    sliderInput("filtro_metrica_valor_z_comp",
                paste("Filter", input$metric_z_comp),
                min = floor(min(values)),
                max = ceiling(max(values)),
                value = c(floor(min(values)), ceiling(max(values))))
  })
  
  # Filtro seleecion jugador para ACWR
    output$filtro_jugador_acwr <- renderUI({
    req(read_data(), input$player_col)
    data <- read_data()
    
    jugadores_unicos <- sort(unique(data[[input$player_col]]))
    
    if (length(jugadores_unicos) >= 12) {
      default_selected <- head(jugadores_unicos, 12)
    } else {
      default_selected <- jugadores_unicos
    }
    
    selectInput(
      "filtro_jugador_acwr", 
      "Jugador:", 
      choices = jugadores_unicos, 
      selected = default_selected,
      multiple = TRUE
    )
  })
  
  # =======================================================
  # 📂 TABLA PRINCIPAL DE DATOS
  # =======================================================
  output$table <- renderDT({
    req(read_data())
    
    datatable(
      read_data(),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'tip',
        class = 'cell-border stripe hover compact',
        autoWidth = TRUE
      ),
      rownames = FALSE,
      class = 'display nowrap cell-border compact stripe'
    ) %>%
      formatStyle(
        columns = names(read_data()),
        backgroundColor = '#1e1e1e',
        color = '#ffffff',
        fontFamily = 'Open Sans',
        fontSize = '14px'
      )
  })
  
  # =======================================================
  # 🧠 FUNCIONES AUXILIARES SEGURAS
  # =======================================================
  
  # Esta función encapsula una condición segura para filtros
  safe_filter_condition <- function(condition) {
    is.logical(condition) && length(condition) == nrow(read_data())
  }
  
  #' Reactive: Filtrado principal de datos
  #'
  #' Esta función reactiva toma el dataset cargado por el usuario (`read_data()`)
  #' y aplica una serie de filtros dinámicos basados en la interfaz de usuario:
  #' - Jugador
  #' - Puesto
  #' - Match Day
  #' - Tarea
  #' - Rango de fechas
  #' - Duración (columna directa o calculada)
  #' - Rango de valores de una métrica seleccionada
  #'
  #' Retorna un dataframe filtrado que se utiliza en el gráfico de barras por fecha.
  filtro_data <- reactive({
    req(read_data())
    data <- read_data()
    
    # ────────────────────────────────────────────────────────────────
    # Filtros categóricos: Jugador, Puesto, Match Day, Tarea
    # ────────────────────────────────────────────────────────────────
    
    # Filtro por jugador
    if (!is.null(input$player_col) && input$player_col %in% colnames(data) &&
        !is.null(input$filtro_jugador) && length(input$filtro_jugador) > 0) {
      data <- data[data[[input$player_col]] %in% input$filtro_jugador, ]
    }
    
    # Filtro por puesto
    if (!is.null(input$position_col) && input$position_col %in% colnames(data) &&
        !is.null(input$filtro_puesto) && length(input$filtro_puesto) > 0) {
      data <- data[data[[input$position_col]] %in% input$filtro_puesto, ]
    }
    
    # Filtro por match day
    if (!is.null(input$matchday_col) && input$matchday_col %in% colnames(data) &&
        !is.null(input$filtro_matchday) && length(input$filtro_matchday) > 0) {
      data <- data[data[[input$matchday_col]] %in% input$filtro_matchday, ]
    }
    
    # Filtro por tarea
    if (!is.null(input$task_col) && input$task_col %in% colnames(data) &&
        !is.null(input$filtro_tarea) && length(input$filtro_tarea) > 0) {
      data <- data[data[[input$task_col]] %in% input$filtro_tarea, ]
    }
    
    # Filtro por fecha
    if (!is.null(input$date_col) && input$date_col %in% colnames(data) &&
        !is.null(input$filtro_fecha) && length(input$filtro_fecha) == 2) {
      fechas <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y")))
      data <- data[!is.na(fechas) & fechas >= input$filtro_fecha[1] & fechas <= input$filtro_fecha[2], ]
    }
    
    # Filtro por duración
    if (!is.null(input$filtro_duracion_input) && length(input$filtro_duracion_input) == 2) {
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
        if (safe_filter_condition(keep)) data <- data[keep, ]
      }
    }
    
    # Filtro por valor de la métrica
    if (!is.null(input$metric) && length(input$metric) == 1 &&
        input$metric %in% colnames(data) &&
        !is.null(input$filtro_metrica_valor) && length(input$filtro_metrica_valor) == 2) {
      metric_vals <- suppressWarnings(as.numeric(data[[input$metric]]))
      keep <- !is.na(metric_vals) & metric_vals >= input$filtro_metrica_valor[1] & metric_vals <= input$filtro_metrica_valor[2]
      if (safe_filter_condition(keep)) data <- data[keep, ]
    }
    
    return(data)
  })
  
  
  #' Reactive: Filtro para gráfico boxplot por Match Day
  #'
  #' Esta función reactiva aplica filtros específicos para el gráfico de boxplot
  #' por Match Day. Utiliza inputs dedicados con sufijo `_box`:
  #' - Jugador (`filtro_jugador_box`)
  #' - Puesto (`filtro_puesto_box`)
  #' - Match Day (`filtro_matchday_box`)
  #' - Tarea (`filtro_tarea_box`)
  #' - Fecha (`filtro_fecha_box`)
  #' - Duración (`filtro_duracion_input_box`)
  #' - Métrica (`metric_box` y `filtro_metrica_valor_box`)
  #'
  #' Retorna un subconjunto de datos filtrado.
  filtro_data_box <- function(metrica, rango) {
    req(read_data())
    data <- read_data()
    
    # ────────────────────────────────────────────────────────────────
    # Filtros categóricos
    # ────────────────────────────────────────────────────────────────
    
    if (!is.null(input$player_col) && input$player_col %in% colnames(data) && !is.null(input$filtro_jugador_box)) {
      data <- data[data[[input$player_col]] %in% input$filtro_jugador_box, ]
    }
    if (!is.null(input$position_col) && input$position_col %in% colnames(data) && !is.null(input$filtro_puesto_box)) {
      data <- data[data[[input$position_col]] %in% input$filtro_puesto_box, ]
    }
    if (!is.null(input$matchday_col) && input$matchday_col %in% colnames(data) && !is.null(input$filtro_matchday_box)) {
      data <- data[data[[input$matchday_col]] %in% input$filtro_matchday_box, ]
    }
    if (!is.null(input$task_col) && input$task_col %in% colnames(data) && !is.null(input$filtro_tarea_box)) {
      data <- data[data[[input$task_col]] %in% input$filtro_tarea_box, ]
    }
    if (!is.null(input$date_col) && input$date_col %in% colnames(data) && !is.null(input$filtro_fecha_box)) {
      fechas <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y")))
      data <- data[!is.na(fechas) & fechas >= input$filtro_fecha_box[1] & fechas <= input$filtro_fecha_box[2], ]
    }
    
    # Filtro por duración
    if (!is.null(input$filtro_duracion_input_box)) {
      dur <- NULL
      if (!is.null(input$duration_col) && input$duration_col != "None" && input$duration_col %in% colnames(data)) {
        dur <- suppressWarnings(as.numeric(data[[input$duration_col]]))
      } else if (!is.null(input$start_col) && !is.null(input$end_col) &&
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
    
    # Filtro por valor de la métrica actual
    if (!is.null(metrica) && metrica %in% colnames(data) && !is.null(rango)) {
      valores <- suppressWarnings(as.numeric(data[[metrica]]))
      keep <- !is.na(valores) & valores >= rango[1] & valores <= rango[2]
      data <- data[keep, ]
    }
    
    return(data)
  }
  
  #' Reactive: Filtro para gráfico boxplot por Tarea
  #'
  #' Esta función reactiva filtra los datos exclusivamente para el gráfico por
  #' tipo de tarea, aplicando los siguientes filtros con sufijo `_task`:
  #' - Jugador (`filtro_jugador_task`)
  #' - Puesto (`filtro_puesto_task`)
  #' - Match Day (`filtro_matchday_task`)
  #' - Tarea (`filtro_tarea_task`)
  #' - Fecha (`filtro_fecha_task`)
  #' - Duración (`filtro_duracion_input_task`)
  #' - Métrica (`metric_task` y `filtro_metrica_valor_task`)
  #'
  #' Retorna los datos filtrados.
  filtro_data_task <- function(metrica, rango) {
    req(read_data())
    data <- read_data()
    
    # ────────────────────────────────────────────────────────────────
    # Filtros categóricos
    # ────────────────────────────────────────────────────────────────
    
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
    
    # Filtrado por valores de la métrica seleccionada
    if (!is.null(metrica) && metrica %in% colnames(data) && !is.null(rango)) {
      valores <- suppressWarnings(as.numeric(data[[metrica]]))
      keep <- !is.na(valores) & valores >= rango[1] & valores <= rango[2]
      data <- data[keep, ]
    }
    
    return(data)
  }
  
  #' Reactive: Filtro de datos para el gráfico de Z-score
  #'
  #' Esta función reactiva aplica los filtros específicos para el gráfico de líneas
  #' de Z-score, utilizando los sufijos `_z` en los inputs. Los filtros incluyen:
  #' - Jugador (`filtro_jugador_z`)
  #' - Puesto (`filtro_puesto_z`)
  #' - Match Day (`filtro_matchday_z`)
  #' - Tarea (`filtro_tarea_z`)
  #' - Fecha (`filtro_fecha_z`)
  #' - Duración (`filtro_duracion_input_z`)
  #' - Métrica (`metric_z` y `filtro_metrica_valor_z`)
  #'
  #' Devuelve un `data.frame` con los datos filtrados listos para calcular z-scores.
  filtro_data_z <- function(metrica, rango) {
    req(read_data())
    data <- read_data()
    # ────────────────────────────────────────────────
    # Filtros categóricos
    # ────────────────────────────────────────────────
    
    if (!is.null(input$player_col) && input$player_col %in% colnames(data) && !is.null(input$filtro_jugador_z)) {
      data <- data[data[[input$player_col]] %in% input$filtro_jugador_z, ]
    }
    if (!is.null(input$position_col) && input$position_col %in% colnames(data) && !is.null(input$filtro_puesto_z)) {
      data <- data[data[[input$position_col]] %in% input$filtro_puesto_z, ]
    }
    if (!is.null(input$matchday_col) && input$matchday_col %in% colnames(data) && !is.null(input$filtro_matchday_z)) {
      data <- data[data[[input$matchday_col]] %in% input$filtro_matchday_z, ]
    }
    if (!is.null(input$task_col) && input$task_col %in% colnames(data) && !is.null(input$filtro_tarea_z)) {
      data <- data[data[[input$task_col]] %in% input$filtro_tarea_z, ]
    }
    if (!is.null(input$date_col) && input$date_col %in% colnames(data) && !is.null(input$filtro_fecha_z)) {
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
    
    # Filtrar por valores de la métrica específica
    if (!is.null(metrica) && metrica %in% colnames(data) && !is.null(rango)) {
      vals <- suppressWarnings(as.numeric(data[[metrica]]))
      keep <- !is.na(vals) & vals >= rango[1] & vals <= rango[2]
      data <- data[keep, ]
    }
    
    return(data)
  }
  
  #' Reactive: Filtro de datos para análisis de sesión específica
  #'
  #' Esta función reactiva aplica todos los filtros relevantes para visualizar
  #' los datos de una o más sesiones específicas seleccionadas manualmente por el usuario.
  #' Los filtros aplicados incluyen jugador, puesto, matchday, tarea, fecha, duración y valores de métrica.
  #'
  #' Devuelve un `data.frame` listo para la visualización del panel de análisis de sesión.
  
  filtro_data_sesion <- reactive({
    req(read_data(), input$filtro_sesion_selector)
    data <- read_data()
    # ────────────────────────────────────────────────
    # Filtros categóricos generales
    # ────────────────────────────────────────────────
    
    # Filtros generales
    if (!is.null(input$player_col) && input$player_col %in% names(data) && !is.null(input$filtro_jugador_sesion)) {
      data <- data[data[[input$player_col]] %in% input$filtro_jugador_sesion, ]
    }
    if (!is.null(input$position_col) && input$position_col %in% names(data) && !is.null(input$filtro_puesto_sesion)) {
      data <- data[data[[input$position_col]] %in% input$filtro_puesto_sesion, ]
    }
    if (!is.null(input$matchday_col) && input$matchday_col %in% names(data) && !is.null(input$filtro_matchday_sesion)) {
      data <- data[data[[input$matchday_col]] %in% input$filtro_matchday_sesion, ]
    }
    if (!is.null(input$task_col) && input$task_col %in% names(data) && !is.null(input$filtro_tarea_sesion)) {
      data <- data[data[[input$task_col]] %in% input$filtro_tarea_sesion, ]
    }
    
    # Filtro por sesión específica
    if (!is.null(input$date_col) && input$date_col %in% names(data)) {
      data[[input$date_col]] <- parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
      if (!is.null(input$filtro_sesion_selector)) {
        data <- data[as.character(data[[input$date_col]]) %in% input$filtro_sesion_selector, ]
      }
    }
    
    # Filtro por duración
    if (!is.null(input$filtro_duracion_input_sesion)) {
      dur <- NULL
      if (!is.null(input$duration_col) && input$duration_col != "None" && input$duration_col %in% names(data)) {
        dur <- suppressWarnings(as.numeric(data[[input$duration_col]]))
      } else if (!is.null(input$start_col) && input$start_col != "None" &&
                 !is.null(input$end_col) && input$end_col != "None" &&
                 input$start_col %in% names(data) && input$end_col %in% names(data)) {
        hora_inicio <- suppressWarnings(parse_time(data[[input$start_col]]))
        hora_fin <- suppressWarnings(parse_time(data[[input$end_col]]))
        dur <- as.numeric(difftime(hora_fin, hora_inicio, units = "mins"))
      }
      if (!is.null(dur)) {
        keep <- !is.na(dur) & dur >= input$filtro_duracion_input_sesion[1] & dur <= input$filtro_duracion_input_sesion[2]
        data <- data[keep, ]
      }
    }
    
    # Filtro por valor de métrica
    if (!is.null(input$metricas_sesion_plot) && length(input$metricas_sesion_plot) > 0 &&
        input$metricas_sesion_plot[1] %in% names(data) &&
        !is.null(input$filtro_metrica_valor_sesion)) {
      metrica <- input$metricas_sesion_plot[1]
      values <- suppressWarnings(as.numeric(data[[metrica]]))
      data <- data[!is.na(values) & values >= input$filtro_metrica_valor_sesion[1] &
                     values <= input$filtro_metrica_valor_sesion[2], ]
    }
    
    return(data)
  })
  
  #' Reactive: Filtro para análisis competitivo
  #'
  #' Esta función filtra los datos para incluir solo sesiones de tipo "Match Day" (MD),
  #' y aplica todos los filtros disponibles: jugador, puesto, tarea, duración, valores, etc.
  #' También asegura que los datos estén ordenados cronológicamente por fecha.
  filtro_data_competitivo <- function(metrica, rango) {
    req(read_data(), input$player_col, input$date_col, input$filtro_sesion_selector_comp)
    data <- read_data()
    
    # ── Filtrar por Match Day = MD ──
    if (!is.null(input$matchday_col) && input$matchday_col %in% names(data)) {
      data[[input$matchday_col]] <- toupper(as.character(data[[input$matchday_col]]))
      data <- data[data[[input$matchday_col]] == "MD", ]
    }
    
    # ── Filtros categóricos ──
    if (!is.null(input$filtro_jugador_z_comp) && input$player_col %in% names(data)) {
      data <- data[data[[input$player_col]] %in% input$filtro_jugador_z_comp, ]
    }
    if (!is.null(input$filtro_puesto_z_comp) && input$position_col %in% names(data)) {
      data <- data[data[[input$position_col]] %in% input$filtro_puesto_z_comp, ]
    }
    if (!is.null(input$filtro_tarea_z_comp) && input$task_col %in% names(data)) {
      data <- data[data[[input$task_col]] %in% input$filtro_tarea_z_comp, ]
    }
    
    # ── Filtro de duración ──
    if (!is.null(input$filtro_duracion_input_z_comp)) {
      dur <- NULL
      if (!is.null(input$duration_col) && input$duration_col != "None" && input$duration_col %in% names(data)) {
        dur <- suppressWarnings(as.numeric(data[[input$duration_col]]))
      } else if (!is.null(input$start_col) && !is.null(input$end_col) &&
                 input$start_col %in% names(data) && input$end_col %in% names(data)) {
        hora_inicio <- suppressWarnings(parse_time(data[[input$start_col]]))
        hora_fin <- suppressWarnings(parse_time(data[[input$end_col]]))
        dur <- as.numeric(difftime(hora_fin, hora_inicio, units = "mins"))
      }
      if (!is.null(dur)) {
        data <- data[!is.na(dur) & dur >= input$filtro_duracion_input_z_comp[1] & dur <= input$filtro_duracion_input_z_comp[2], ]
      }
    }
    
    # ── Filtro por sesión específica (fecha seleccionada) ──
    if (!is.null(input$filtro_sesion_selector_comp) &&
        !is.null(input$date_col) && input$date_col %in% names(data)) {
      data[[input$date_col]] <- parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
      data <- data[as.character(data[[input$date_col]]) == input$filtro_sesion_selector_comp, ]
    }
    
    # ── Filtro por valor de la métrica ──
    if (!is.null(metrica) && metrica %in% names(data) && !is.null(rango)) {
      vals <- suppressWarnings(as.numeric(data[[metrica]]))
      keep <- !is.na(vals) & is.finite(vals) & vals >= rango[1] & vals <= rango[2]
      data <- data[keep, ]
    }
    
    data <- data[order(data[[input$player_col]], data[[input$date_col]]), ]
    return(data)
  }
  
  #' 🔵 Reactive: Filtro para datos de ACWR
  #'
  #' Aplica filtros específicos para el gráfico de ACWR:
  #' - Jugador (`filtro_jugador_acwr`)
  #' - Puesto (`filtro_puesto_acwr`)
  #' - Match Day (`filtro_matchday_acwr`)
  #' - Tarea (`filtro_tarea_acwr`)
  #' - Fecha (`filtro_fecha_acwr`)
  #' - Duración (`filtro_duracion_input_acwr`)
  #' - Rango de valores de cada métrica (`filtro_metrica_valor_acwr`)
  #'
  #' Retorna un dataframe filtrado listo para calcular ACWR.
  filtro_data_acwr <- function(metrica, rango) {
    req(read_data())
    data <- read_data()
    
    # Filtros categóricos
    if (!is.null(input$player_col) && input$player_col %in% colnames(data) && !is.null(input$filtro_jugador_acwr)) {
      data <- data[data[[input$player_col]] %in% input$filtro_jugador_acwr, ]
    }
    if (!is.null(input$position_col) && input$position_col %in% colnames(data) && !is.null(input$filtro_puesto_acwr)) {
      data <- data[data[[input$position_col]] %in% input$filtro_puesto_acwr, ]
    }
    if (!is.null(input$matchday_col) && input$matchday_col %in% colnames(data) && !is.null(input$filtro_matchday_acwr)) {
      data <- data[data[[input$matchday_col]] %in% input$filtro_matchday_acwr, ]
    }
    if (!is.null(input$task_col) && input$task_col %in% colnames(data) && !is.null(input$filtro_tarea_acwr)) {
      data <- data[data[[input$task_col]] %in% input$filtro_tarea_acwr, ]
    }
    
    # Filtro por fecha
    if (!is.null(input$date_col) && input$date_col %in% colnames(data) && !is.null(input$filtro_fecha_acwr)) {
      fechas <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y")))
      data <- data[!is.na(fechas) & fechas >= input$filtro_fecha_acwr[1] & fechas <= input$filtro_fecha_acwr[2], ]
    }
    
    # Filtro por duración
    if (!is.null(input$filtro_duracion_input_acwr)) {
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
        keep <- !is.na(dur) & dur >= input$filtro_duracion_input_acwr[1] & dur <= input$filtro_duracion_input_acwr[2]
        data <- data[keep, ]
      }
    }
    
    # Filtro de valores de la métrica
    if (!is.null(metrica) && metrica %in% names(data) && !is.null(rango)) {
      valores <- suppressWarnings(as.numeric(data[[metrica]]))
      keep <- !is.na(valores) & valores >= rango[1] & valores <= rango[2]
      data <- data[keep, ]
    }
    
    return(data)
  }
  
  # 🧠 UI dinámico: Gráfico + Filtro por cada métrica seleccionada metrica en el tiempo
  output$barras_fecha_ui <- renderUI({
    req(input$metric, read_data())
    
    ui_blocks <- lapply(input$metric, function(metrica) {
      metrica_clean <- make.names(metrica)
      values <- suppressWarnings(as.numeric(read_data()[[metrica]]))
      values <- values[!is.na(values) & is.finite(values)]
      
      tagList(
        tags$hr(),
        tags$h4(paste("Gráfico y Filtro:", metrica)),
        sliderInput(
          inputId = paste0("filtro_metrica_valor_", metrica_clean),
          label = paste("Filtro de valores para", metrica),
          min = floor(min(values)),
          max = ceiling(max(values)),
          value = c(floor(min(values)), ceiling(max(values)))
        ),
        plotlyOutput(outputId = paste0("barras_fecha_plot_", metrica_clean), height = "400px")
      )
    })
    
    do.call(tagList, ui_blocks)
  })
  
  
  # 🧠 UI dinámico: Gráfico + Filtro por cada métrica seleccionada boxplot por MD
  output$boxplot_matchday_ui <- renderUI({
    req(input$metric_box, read_data())
    
    lapply(input$metric_box, function(metrica) {
      metrica_clean <- make.names(metrica)
      values <- suppressWarnings(as.numeric(read_data()[[metrica]]))
      values <- values[!is.na(values) & is.finite(values)]
      
      tagList(
        tags$hr(),
        tags$h4(paste("Boxplot:", metrica)),
        sliderInput(
          inputId = paste0("filtro_metrica_valor_box_", metrica_clean),
          label = paste("Filtro de valores para", metrica),
          min = floor(min(values)),
          max = ceiling(max(values)),
          value = c(floor(min(values)), ceiling(max(values)))
        ),
        plotlyOutput(outputId = paste0("boxplot_matchday_plot_", metrica_clean), height = "400px")
      )
    }) |> tagList()
  })
  
  # 🧠 UI dinámico: Gráfico + Filtro por cada métrica seleccionada boxplot por Tarea
  output$boxplot_task_ui <- renderUI({
    req(input$metric_task, read_data())
    
    lapply(input$metric_task, function(metrica) {
      metrica_clean <- make.names(metrica)
      values <- suppressWarnings(as.numeric(read_data()[[metrica]]))
      values <- values[!is.na(values) & is.finite(values)]
      
      tagList(
        tags$hr(),
        tags$h4(paste("Boxplot:", metrica)),
        sliderInput(
          inputId = paste0("filtro_metrica_valor_task_", metrica_clean),
          label = paste("Filtro de valores para", metrica),
          min = floor(min(values)),
          max = ceiling(max(values)),
          value = c(floor(min(values)), ceiling(max(values)))
        ),
        plotlyOutput(outputId = paste0("boxplot_task_plot_", metrica_clean), height = "400px")
      )
    }) |> tagList()
  })
  
  # 🧠 UI dinámico: Gráfico + Filtro para cada métrica seleccionada en el tab de Z-score
  output$zscore_plot_ui <- renderUI({
    req(input$metric_z, read_data())
    
    lapply(input$metric_z, function(metrica) {
      metrica_clean <- make.names(metrica)
      values <- suppressWarnings(as.numeric(read_data()[[metrica]]))
      values <- values[!is.na(values) & is.finite(values)]
      
      tagList(
        tags$hr(),
        tags$h4(paste("Z-score:", metrica)),
        sliderInput(
          inputId = paste0("filtro_metrica_valor_z_", metrica_clean),
          label = paste("Filtro de valores para", metrica),
          min = floor(min(values)),
          max = ceiling(max(values)),
          value = c(floor(min(values)), ceiling(max(values)))
        ),
        plotlyOutput(outputId = paste0("zscore_plot_", metrica_clean), height = "400px")
      )
    }) |> tagList()
  })
  
  # 🧠 UI dinámico: Gráfico + Filtro por cada métrica seleccionada Analisis competitivo
  output$zscore_comp_plot_ui <- renderUI({
    req(input$metric_z_comp, read_data())
    
    lapply(input$metric_z_comp, function(metrica) {
      metrica_clean <- make.names(metrica)
      values <- suppressWarnings(as.numeric(read_data()[[metrica]]))
      values <- values[!is.na(values) & is.finite(values)]
      
      tagList(
        tags$hr(),
        tags$h4(paste("Z-score competitivo:", metrica)),
        sliderInput(
          inputId = paste0("filtro_metrica_valor_z_comp_", metrica_clean),
          label = paste("Filtro de valores para", metrica),
          min = floor(min(values)),
          max = ceiling(max(values)),
          value = c(floor(min(values)), ceiling(max(values)))
        ),
        plotlyOutput(outputId = paste0("zscore_comp_plot_", metrica_clean), height = "400px")
      )
    }) |> tagList()
  })
  
  # 🧠 UI dinámico: Gráfico + Filtro por cada métrica seleccionada en ACWR
  # 🔵 Output: UI dinámica para ACWR
  output$acwr_plot_ui <- renderUI({
    req(input$metric_acwr, read_data())
    
    lapply(input$metric_acwr, function(metrica) {
      metrica_clean <- make.names(metrica)
      values <- suppressWarnings(as.numeric(read_data()[[metrica]]))
      values <- values[!is.na(values) & is.finite(values)]
      
      tagList(
        tags$hr(),
        tags$h4(paste("ACWR:", metrica)),
        sliderInput(
          inputId = paste0("filtro_metrica_valor_acwr_", metrica_clean),
          label = paste("Filtro de valores para", metrica),
          min = floor(min(values)),
          max = ceiling(max(values)),
          value = c(floor(min(values)), ceiling(max(values)))
        ),
        plotlyOutput(outputId = paste0("acwr_plot_", metrica_clean), height = "400px")
      )
    }) |> tagList()
  })
  
  #' Output: Gráfico de barras por fecha (Promedios por jugador)
  #'
  #' Visualiza la evolución diaria del valor promedio de la métrica seleccionada
  #' para cada jugador. Se agrupa por `date_col` y `player_col`, y se genera un gráfico
  #' con `ggplot2` + `plotly`, con barras apiladas por jugador.
  # 🧠 OBSERVE: renderiza un gráfico por cada métrica seleccionada en "Métrica en el tiempo"
  observe({
    req(input$metric)
    
    for (metrica in input$metric) {
      local({
        metrica_local <- metrica
        metrica_clean <- make.names(metrica_local)
        plot_id <- paste0("barras_fecha_plot_", metrica_clean)
        filtro_id <- paste0("filtro_metrica_valor_", metrica_clean)
        
        output[[plot_id]] <- renderPlotly({
          req(filtro_data(), input[[filtro_id]])
          data <- filtro_data()
          
          data[[metrica_local]] <- suppressWarnings(as.numeric(data[[metrica_local]]))
          data[[input$date_col]] <- parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
          data <- data[!is.na(data[[metrica_local]]) & !is.na(data[[input$date_col]]), ]
          
          # Aplicar filtro de valores de esta métrica específica
          val_range <- input[[filtro_id]]
          data <- data[data[[metrica_local]] >= val_range[1] & data[[metrica_local]] <= val_range[2], ]
          
          plot_data <- data %>%
            group_by(Fecha = as.Date(.data[[input$date_col]]), Jugador = .data[[input$player_col]]) %>%
            summarise(Promedio = mean(.data[[metrica_local]], na.rm = TRUE), .groups = "drop") %>%
            mutate(
              Fecha = factor(Fecha, levels = sort(unique(Fecha))),
              tooltip = paste0("Jugador: ", Jugador, "<br>Fecha: ", Fecha, "<br>Promedio: ", round(Promedio, 2))
            )
          p <- ggplot(plot_data, aes(x = Fecha, y = Promedio, fill = Jugador, text = tooltip)) +
            geom_col(position = position_dodge2(preserve = "single"), width = 0.7) +
            scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)]) +
            scale_fill_manual(values = rep("#fd002b", length(unique(plot_data$Jugador)))) +
            labs(
              title = paste("Promedio de", metrica_local, "por Fecha y Jugador"),
              x = "Fecha", y = metrica_local
            ) +
            theme_minimal(base_size = 14) +
            theme(
              plot.background = element_rect(fill = "#1e1e1e", color = NA),
              panel.background = element_rect(fill = "#1e1e1e", color = NA),
              panel.grid.major = element_line(color = "#2c2c2c"),
              panel.grid.minor = element_line(color = "#2c2c2c"),
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, color = "#ffffff"),
              axis.text.y = element_text(size = 12, color = "#ffffff"),
              axis.title = element_text(face = "bold", size = 14, color = "#ffffff"),
              plot.title = element_text(
                hjust = 0.5, face = "bold", size = 20, color = "#fd002b",
                family = "Righteous"
              ),
              legend.position = "none",
              legend.text = element_text(color = "#ffffff"),
              legend.title = element_text(color = "#ffffff", face = "bold")
            )
          
          ggplotly(p, tooltip = "text") %>%
            layout(
              plot_bgcolor = "#1e1e1e",
              paper_bgcolor = "#1e1e1e",
              font = list(color = "#ffffff")
            )
        })
      })
    }
  })
  
  #' Output: Gráfico Boxplot por Match Day
  #'
  #' Este gráfico muestra la distribución de una métrica seleccionada (`input$metric_box`)
  #' para cada `Match Day`, agrupando por jugador. Utiliza `ggplot2` y `plotly` para visualización
  #' interactiva. Los datos se filtran con `filtro_data_box()`, respetando filtros locales del tabPanel.
  observe({
    req(input$metric_box, read_data())
    
    for (metrica in input$metric_box) {
      local({
        metrica_local <- metrica
        metrica_clean <- make.names(metrica_local)
        plot_id <- paste0("boxplot_matchday_plot_", metrica_clean)
        filtro_id <- paste0("filtro_metrica_valor_box_", metrica_clean)
        
        output[[plot_id]] <- renderPlotly({
          data <- filtro_data_box(metrica_local, input[[filtro_id]])
          req(data, input$matchday_col, input$player_col, metrica_local, input[[filtro_id]])
          
          data[[metrica_local]] <- suppressWarnings(as.numeric(data[[metrica_local]]))
          val_range <- input[[filtro_id]]
          data <- data[!is.na(data[[metrica_local]]) & data[[metrica_local]] >= val_range[1] & data[[metrica_local]] <= val_range[2], ]
          
          plot_data <- data %>%
            mutate(
              MatchDay = as.factor(.data[[input$matchday_col]]),
              Jugador = .data[[input$player_col]],
              Valor = .data[[metrica_local]],
              tooltip = paste0("Jugador: ", Jugador, "<br>Match Day: ", MatchDay, "<br>", metrica_local, ": ", round(Valor, 2))
            )
          
          p <- ggplot(plot_data, aes(x = MatchDay, y = Valor, text = tooltip)) +
            geom_boxplot(
              aes(fill = MatchDay),
              outlier.shape = 21,             # Forma con borde + relleno
              outlier.size = 2,
              outlier.fill = "#ffffff",       # Color interior blanco
              outlier.color = "#ffffff",      # Borde también blanco (o "#fd002b" si querés contraste)
              alpha = 0.7
            ) +
            scale_fill_manual(values = rep("#fd002b", length(unique(plot_data$MatchDay)))) +
            theme_minimal(base_size = 14) +
            labs(
              title = paste("Distribución de", metrica_local, "por Match Day"),
              x = "Match Day", y = metrica_local
            ) +
            theme(
              plot.background = element_rect(fill = "#1e1e1e", color = NA),
              panel.background = element_rect(fill = "#1e1e1e", color = NA),
              panel.grid.major = element_line(color = "#2c2c2c"),
              panel.grid.minor = element_line(color = "#2c2c2c"),
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, color = "#ffffff"),
              axis.text.y = element_text(size = 12, color = "#ffffff"),
              axis.title = element_text(face = "bold", size = 14, color = "#ffffff"),
              plot.title = element_text(
                hjust = 0.5, face = "bold", size = 20, color = "#fd002b",
                family = "Righteous"
              ),
              legend.position = "none"
            )
          
          ggplotly(p, tooltip = "text")
        })
      })
    }
  })
  
  #' Output: Gráfico Boxplot por Tarea
  #'
  #' Este gráfico muestra la distribución de una métrica seleccionada (`input$metric_task`)
  #' para cada tarea definida en `input$task_col`, agrupando por jugador. Usa `filtro_data_task()`
  #' para aplicar los filtros correspondientes y `ggplotly()` para generar una visualización interactiva.
  observe({
    req(input$metric_task)
    
    for (metrica in input$metric_task) {
      local({
        metrica_local <- metrica
        metrica_clean <- make.names(metrica_local)
        plot_id <- paste0("boxplot_task_plot_", metrica_clean)
        filtro_id <- paste0("filtro_metrica_valor_task_", metrica_clean)
        
        output[[plot_id]] <- renderPlotly({
          req(input[[filtro_id]])
          data <- filtro_data_task(metrica_local, input[[filtro_id]])
          
          data[[metrica_local]] <- suppressWarnings(as.numeric(data[[metrica_local]]))
          val_range <- input[[filtro_id]]
          data <- data[!is.na(data[[metrica_local]]) & data[[metrica_local]] >= val_range[1] & data[[metrica_local]] <= val_range[2], ]
          
          plot_data <- data %>%
            mutate(
              Tarea = as.factor(.data[[input$task_col]]),
              Jugador = .data[[input$player_col]],
              Valor = .data[[metrica_local]],
              tooltip = paste0("Jugador: ", Jugador, "<br>Tarea: ", Tarea, "<br>", metrica_local, ": ", round(Valor, 2))
            )
          
          p <- ggplot(plot_data, aes(x = Tarea, y = Valor, text = tooltip)) +
            geom_boxplot(
              aes(fill = Tarea),
              outlier.shape = 21,
              outlier.size = 2,
              outlier.fill = "#ffffff",
              outlier.color = "#ffffff",
              alpha = 0.7
            ) +
            scale_fill_manual(values = rep("#fd002b", length(unique(plot_data$Tarea)))) +
            theme_minimal(base_size = 14) +
            labs(
              title = paste("Distribución de", metrica_local, "por Tarea"),
              x = "Tarea", y = metrica_local
            ) +
            theme(
              plot.background = element_rect(fill = "#1e1e1e", color = NA),
              panel.background = element_rect(fill = "#1e1e1e", color = NA),
              panel.grid.major = element_line(color = "#2c2c2c"),
              panel.grid.minor = element_line(color = "#2c2c2c"),
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, color = "#ffffff"),
              axis.text.y = element_text(size = 12, color = "#ffffff"),
              axis.title = element_text(face = "bold", size = 14, color = "#ffffff"),
              plot.title = element_text(
                hjust = 0.5, face = "bold", size = 20, color = "#fd002b",
                family = "Righteous"
              ),
              legend.position = "none"
            )
          
          ggplotly(p, tooltip = "text")
        })
      })
    }
  })
  
  #' Output: Gráfico de Z-score por jugador
  #'
  #' Este gráfico muestra la evolución del Z-score por jugador para una métrica seleccionada,
  #' calculado a partir de la media y desvío estándar global por jugador (no media móvil).
  #' Las facetas muestran un gráfico individual por jugador, y se colorea según valores altos,
  #' bajos o neutros. Incluye loess smoothing por jugador y áreas coloreadas para zonas críticas.
  observe({
    req(input$metric_z)
    
    for (metrica in input$metric_z) {
      local({
        metrica_local <- metrica
        metrica_clean <- make.names(metrica_local)
        plot_id <- paste0("zscore_plot_", metrica_clean)
        filtro_id <- paste0("filtro_metrica_valor_z_", metrica_clean)
        
        output[[plot_id]] <- renderPlotly({
          req(input[[filtro_id]], input$player_col, input$date_col)
          
          # Llamamos a filtro_data_z pasando los argumentos correctos
          data <- filtro_data_z(metrica = metrica_local, rango = input[[filtro_id]])
          
          # Validación
          if (!(metrica_local %in% names(data))) return(NULL)
          
          data[[input$date_col]] <- parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
          data[[metrica_local]] <- suppressWarnings(as.numeric(data[[metrica_local]]))
          data <- data[!is.na(data[[input$date_col]]) & !is.na(data[[metrica_local]]), ]
          
          
          # Filtrar por rango de valores
          val_range <- input[[filtro_id]]
          data <- data[data[[metrica_local]] >= val_range[1] & data[[metrica_local]] <= val_range[2], ]
          
          # Calcular z-score por jugador
          z_data <- data %>%
            arrange(.data[[input$player_col]], .data[[input$date_col]]) %>%
            group_by(Jugador = .data[[input$player_col]]) %>%
            mutate(
              Valor = .data[[metrica_local]],
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
          
          # Filtrar jugadores: mostrar solo 12 por defecto si no se seleccionó ninguno
          if (!is.null(input$filtro_jugador_z) && length(input$filtro_jugador_z) > 0) {
            z_data <- z_data %>% filter(Jugador %in% input$filtro_jugador_z)
          } else {
            jugadores_default <- unique(z_data$Jugador)[1:min(12, length(unique(z_data$Jugador)))]
            z_data <- z_data %>% filter(Jugador %in% jugadores_default)
          }
          
          if (nrow(z_data) == 0) return(NULL)
          
          # Colores
          colores_base <- c("Alto" = "#e74c3c", "Bajo" = "#2ecc71", "Neutral" = "#f1c40f")
          fondo_rojo  <- "#fdecea"
          fondo_verde <- "#eafaf1"
          fecha_min <- min(z_data$Fecha, na.rm = TRUE)
          fecha_max <- max(z_data$Fecha, na.rm = TRUE)
          
          # Número de columnas dinámico (máximo 4 por fila)
          jugadores_unicos <- length(unique(z_data$Jugador))
          ncol_facetas <- min(4, jugadores_unicos)
          
          # Gráfico
          p <- ggplot(z_data, aes(x = Fecha, y = z, text = tooltip, color = z_color)) +
            annotate("rect", xmin = fecha_min, xmax = fecha_max,
                     ymin = 1.5, ymax = Inf, fill = fondo_rojo, alpha = 0.4) +
            annotate("rect", xmin = fecha_min, xmax = fecha_max,
                     ymin = -Inf, ymax = -1.5, fill = fondo_verde, alpha = 0.4) +
            geom_smooth(aes(group = Jugador), method = "loess", span = 0.9, se = FALSE, color = "#34495e", linewidth = 0.6) +
            geom_point(size = 1.2) +
            scale_color_manual(values = colores_base, name = "Z-score") +
            geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
            facet_wrap(~Jugador, scales = "free_y", ncol = 4) +
            theme_minimal(base_size = 14) +
            labs(
              title = paste("Z-score de", metrica_local, "por jugador"),
              x = "Fecha", y = "Z-score"
            ) +
            theme(
              plot.background = element_rect(fill = "#1e1e1e", color = NA),
              panel.background = element_rect(fill = "#1e1e1e", color = NA),
              panel.grid.major = element_line(color = "#2c2c2c"),
              panel.grid.minor = element_line(color = "#2c2c2c"),
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, color = "#ffffff"),
              axis.text.y = element_text(size = 12, color = "#ffffff"),
              axis.title = element_text(face = "bold", size = 14, color = "#ffffff"),
              plot.title = element_text(
                hjust = 0.5, face = "bold", size = 20, color = "#fd002b",
                family = "Righteous"
              ),
              strip.text = element_text(face = "bold", size = 13, color = "#ffffff"),
              legend.position = "bottom",
              legend.text = element_text(color = "#ffffff"),
              legend.title = element_text(color = "#ffffff", face = "bold")
            )
          
          ggplotly(p, tooltip = "text") %>%
            layout(legend = list(orientation = "h", x = 0.3, y = -0.2))
        })
      })
    }
  })
  
  #' Output: Gráfico de barras ordenadas por jugador en sesión
  #'
  #' Este gráfico muestra el valor promedio de una métrica seleccionada por jugador para una sesión específica.
  #' Permite identificar visualmente qué jugadores están por encima o por debajo del promedio o ±1 SD.
  #' Las barras se ordenan de menor a mayor valor de la métrica. Se incluyen líneas de referencia para la media
  #' y la desviación estándar, junto con una franja sombreada que representa el rango ±1 SD.
  # Renderiza un contenedor UI que aloja múltiples gráficos de sesión
  output$graficos_metricas_sesion <- renderUI({
    req(input$metricas_sesion_plot, read_data())
    
    lapply(input$metricas_sesion_plot, function(metrica) {
      metrica_clean <- make.names(metrica)
      values <- suppressWarnings(as.numeric(read_data()[[metrica]]))
      values <- values[!is.na(values) & is.finite(values)]
      
      tagList(
        tags$hr(),
        tags$h4(paste("Sesión –", metrica)),
        
        # Filtro individual para la métrica
        sliderInput(
          inputId = paste0("filtro_metrica_valor_sesion_", metrica_clean),
          label = paste("Filtrar valores de", metrica),
          min = floor(min(values)),
          max = ceiling(max(values)),
          value = c(floor(min(values)), ceiling(max(values)))
        ),
        
        # Gráfico
        plotlyOutput(paste0("plot_sesion_", metrica_clean), height = "400px")
      )
    }) |> tagList()
  })
  
  # Renderiza cada gráfico individualmente
  observe({
    req(input$metricas_sesion_plot)
    
    for (metrica in input$metricas_sesion_plot) {
      local({
        metrica_local <- metrica
        metrica_clean <- make.names(metrica_local)
        plot_id <- paste0("plot_sesion_", metrica_clean)
        filtro_id <- paste0("filtro_metrica_valor_sesion_", metrica_clean)
        
        output[[plot_id]] <- renderPlotly({
          req(filtro_data_sesion(), input$player_col, input[[filtro_id]])
          data <- filtro_data_sesion()
          
          if (!(metrica_local %in% names(data))) return(NULL)
          data[[metrica_local]] <- suppressWarnings(as.numeric(data[[metrica_local]]))
          
          # Aplicar filtro individual por métrica
          val_range <- input[[filtro_id]]
          data <- data[!is.na(data[[metrica_local]]) & 
                         data[[metrica_local]] >= val_range[1] & 
                         data[[metrica_local]] <= val_range[2], ]
          
          resumen <- data %>%
            group_by(Jugador = .data[[input$player_col]]) %>%
            summarise(Valor = mean(.data[[metrica_local]], na.rm = TRUE), .groups = "drop") %>%
            arrange(Valor) %>%
            mutate(Jugador = factor(Jugador, levels = Jugador))
          
          media <- mean(resumen$Valor, na.rm = TRUE)
          sd_val <- sd(resumen$Valor, na.rm = TRUE)
          
          p <- ggplot(resumen, aes(x = Jugador, y = Valor, fill = Valor, text = paste0("Jugador: ", Jugador, "<br>Valor: ", round(Valor, 2)))) +
            annotate("rect", xmin = -Inf, xmax = Inf,
                     ymin = media - sd_val, ymax = media + sd_val,
                     alpha = 0.2, fill = "#2c2c2c") +
            geom_col(show.legend = FALSE) +
            geom_hline(yintercept = media, linetype = "dashed", color = "#ffffff", linewidth = 1) +
            geom_hline(yintercept = media + sd_val, linetype = "dotted", color = "#ffffff", linewidth = 0.8) +
            geom_hline(yintercept = media - sd_val, linetype = "dotted", color = "#ffffff", linewidth = 0.8) +
            geom_hline(yintercept = media + 2 * sd_val, linetype = "dotted", color = "#888888", linewidth = 0.8) +
            geom_hline(yintercept = media - 2 * sd_val, linetype = "dotted", color = "#888888", linewidth = 0.8) +
            scale_fill_gradient(low = "#3498db", high = "#e74c3c") +
            theme_minimal(base_size = 14) +
            labs(
              title = paste("Valores de", metrica_local, "por jugador – Sesión"),
              y = metrica_local, x = "Jugador"
            ) +
            theme(
              plot.background = element_rect(fill = "#1e1e1e", color = NA),
              panel.background = element_rect(fill = "#1e1e1e", color = NA),
              panel.grid.major = element_line(color = "#2c2c2c"),
              panel.grid.minor = element_line(color = "#2c2c2c"),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "#ffffff"),
              axis.text.y = element_text(size = 12, color = "#ffffff"),
              axis.title = element_text(face = "bold", size = 14, color = "#ffffff"),
              plot.title = element_text(
                hjust = 0.5, face = "bold", size = 20, color = "#fd002b",
                family = "Righteous"
              )
            )
          
          ggplotly(p, tooltip = "text")
        })
      })
    }
  })
  #' Output: Gráfico de Z-score competitivo
  #'
  #' Este gráfico compara el valor del último partido con la media móvil y SD de los 3-5
  #' partidos anteriores, sin incluir el partido actual. Se muestra un gráfico por jugador.
  # Output: Gráfico de Z-score competitivo
  # UI dinámico para múltiples métricas en Z-score competitivo
  output$zscore_comp_plot <- renderUI({
    req(input$metric_z_comp, read_data())
    
    plots <- lapply(input$metric_z_comp, function(metrica) {
      metrica_clean <- make.names(metrica)
      plot_id <- paste0("zscore_comp_plot_", metrica_clean)
      plotlyOutput(plot_id, height = "500px")
    })
    
    do.call(tagList, plots)
  })
  
  # RenderPlotly para cada métrica seleccionada en el panel competitivo
  observe({
    req(input$metric_z_comp)
    
    for (metrica in input$metric_z_comp) {
      local({
        metrica_local <- metrica
        metrica_clean <- make.names(metrica_local)
        filtro_id <- paste0("filtro_metrica_valor_z_comp_", metrica_clean)
        plot_id <- paste0("zscore_comp_plot_", metrica_clean)
        
        output[[plot_id]] <- renderPlotly({
          req(read_data(), input$player_col, input$date_col, input$filtro_sesion_selector_comp, input[[filtro_id]])
          
          data_full <- read_data()
          player_col <- input$player_col
          date_col <- input$date_col
          window_size <- input$ventana_movil_z_comp
          
          # Parsear fecha
          data_full[[date_col]] <- parse_date_time(data_full[[date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
          data_full <- data_full[!is.na(data_full[[date_col]]), ]
          
          # Match Day = MD
          if (!is.null(input$matchday_col) && input$matchday_col %in% names(data_full)) {
            data_full[[input$matchday_col]] <- toupper(as.character(data_full[[input$matchday_col]]))
            data_full <- data_full[data_full[[input$matchday_col]] == "MD", ]
          }
          
          # Filtros categóricos
          data_full <- data_full %>%
            filter(
              is.finite(.data[[metrica_local]]),
              if (!is.null(input$filtro_jugador_z_comp)) .data[[player_col]] %in% input$filtro_jugador_z_comp else TRUE,
              if (!is.null(input$filtro_puesto_z_comp) && input$position_col %in% names(.)) .data[[input$position_col]] %in% input$filtro_puesto_z_comp else TRUE,
              if (!is.null(input$filtro_tarea_z_comp) && input$task_col %in% names(.)) .data[[input$task_col]] %in% input$filtro_tarea_z_comp else TRUE
            )
          
          # Filtro por duración
          if (!is.null(input$filtro_duracion_input_z_comp)) {
            dur <- NULL
            if (!is.null(input$duration_col) && input$duration_col != "None" && input$duration_col %in% names(data_full)) {
              dur <- suppressWarnings(as.numeric(data_full[[input$duration_col]]))
            } else if (!is.null(input$start_col) && !is.null(input$end_col) &&
                       input$start_col %in% names(data_full) && input$end_col %in% names(data_full)) {
              hora_inicio <- suppressWarnings(parse_time(data_full[[input$start_col]]))
              hora_fin <- suppressWarnings(parse_time(data_full[[input$end_col]]))
              dur <- as.numeric(difftime(hora_fin, hora_inicio, units = "mins"))
            }
            if (!is.null(dur)) {
              keep <- !is.na(dur) & dur >= input$filtro_duracion_input_z_comp[1] & dur <= input$filtro_duracion_input_z_comp[2]
              data_full <- data_full[keep, ]
            }
          }
          
          # Filtro por valores de la métrica actual
          if (!is.null(input[[filtro_id]]) && metrica_local %in% names(data_full)) {
            vals <- suppressWarnings(as.numeric(data_full[[metrica_local]]))
            keep <- !is.na(vals) & vals >= input[[filtro_id]][1] & vals <= input[[filtro_id]][2]
            data_full <- data_full[keep, ]
          }
          
          # Calcular rolling stats anteriores
          fecha_partido <- parse_date_time(input$filtro_sesion_selector_comp, orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
          
          stats_movil <- data_full %>%
            filter(.data[[date_col]] < fecha_partido) %>%
            arrange(.data[[player_col]], .data[[date_col]]) %>%
            group_by(Jugador = .data[[player_col]]) %>%
            summarise(
              media_movil = if (n() >= window_size) mean(tail(.data[[metrica_local]], window_size), na.rm = TRUE) else NA_real_,
              sd_movil = if (n() >= window_size) sd(tail(.data[[metrica_local]], window_size), na.rm = TRUE) else NA_real_,
              .groups = "drop"
            )
          
          # Datos del partido actual
          data_sesion <- data_full %>%
            filter(as.character(.data[[date_col]]) == input$filtro_sesion_selector_comp) %>%
            mutate(Jugador = .data[[player_col]]) %>%
            select(Jugador, Fecha = .data[[date_col]], Valor = .data[[metrica_local]])
          
          # Calcular Z-score
          data_final <- left_join(data_sesion, stats_movil, by = "Jugador") %>%
            mutate(
              z = (Valor - media_movil) / sd_movil
            ) %>%
            filter(!is.na(z) & is.finite(z)) %>%
            group_by(Jugador) %>%
            summarise(
              z = mean(z, na.rm = TRUE),
              z_color = case_when(
                z >= 1.5 ~ "Alto",
                z <= -1.5 ~ "Bajo",
                TRUE ~ "Neutral"
              ),
              tooltip = paste0("Jugador: ", Jugador, "<br>Z-score: ", round(z, 2)),
              .groups = "drop"
            ) %>%
            arrange(z) %>%
            mutate(Jugador = factor(Jugador, levels = unique(Jugador)))  # Evita duplicados
          
          if (nrow(data_final) == 0 || all(is.na(data_final$z))) {
            return(plotly_empty(type = "bar") %>% layout(title = "No hay datos suficientes para mostrar el gráfico."))
          }
          
          colores <- c("Alto" = "#e74c3c", "Bajo" = "#2ecc71", "Neutral" = "#f1c40f")
          
          p <- ggplot(data_final, aes(x = Jugador, y = z, fill = z_color, text = tooltip)) +
            geom_col(width = 0.6) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "#ffffff") +
            geom_hline(yintercept = 1.5, linetype = "dotted", color = "#ffffff", linewidth = 0.8) +
            geom_hline(yintercept = -1.5, linetype = "dotted", color = "#ffffff", linewidth = 0.8) +
            scale_fill_manual(values = colores, name = "Z-score") +
            theme_minimal(base_size = 14) +
            labs(
              title = paste("Z-score – Partido del", input$filtro_sesion_selector_comp),
              x = "Jugador", y = "Z-score"
            ) +
            theme(
              plot.background = element_rect(fill = "#1e1e1e", color = NA),
              panel.background = element_rect(fill = "#1e1e1e", color = NA),
              panel.grid.major = element_line(color = "#2c2c2c"),
              panel.grid.minor = element_line(color = "#2c2c2c"),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "#ffffff"),
              axis.text.y = element_text(size = 12, color = "#ffffff"),
              axis.title = element_text(face = "bold", size = 14, color = "#ffffff"),
              plot.title = element_text(
                hjust = 0.5, face = "bold", size = 20, color = "#fd002b",
                family = "Righteous"
              ),
              legend.position = "right",
              legend.text = element_text(color = "#ffffff"),
              legend.title = element_text(color = "#ffffff", face = "bold")
            )
          
          ggplotly(p, tooltip = "text")
        })
      })
    }
  })
  #' Output: Tabla resumen competitivo
  #'
  #' Muestra los valores del último partido por jugador, junto con la media y SD móvil
  #' de los partidos anteriores, y el Z-score comparativo.
  output$tabla_resumen_comp <- renderDT({
    req(read_data(), input$metric_z_comp, input$player_col, input$date_col, input$filtro_sesion_selector_comp)
    
    data_full <- read_data()
    metrics <- input$metric_z_comp
    player_col <- input$player_col
    date_col <- input$date_col
    window_size <- input$ventana_movil_z_comp
    
    # Parsear fechas
    data_full[[date_col]] <- parse_date_time(data_full[[date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
    data_full <- data_full[!is.na(data_full[[date_col]]), ]
    
    # Filtrar solo MD
    if (!is.null(input$matchday_col) && input$matchday_col %in% names(data_full)) {
      data_full[[input$matchday_col]] <- toupper(as.character(data_full[[input$matchday_col]]))
      data_full <- data_full[data_full[[input$matchday_col]] == "MD", ]
    }
    
    # Filtros categóricos
    data_full <- data_full %>%
      filter(
        if (!is.null(input$filtro_jugador_z_comp)) .data[[player_col]] %in% input$filtro_jugador_z_comp else TRUE,
        if (!is.null(input$filtro_puesto_z_comp) && input$position_col %in% names(.)) .data[[input$position_col]] %in% input$filtro_puesto_z_comp else TRUE,
        if (!is.null(input$filtro_tarea_z_comp) && input$task_col %in% names(.)) .data[[input$task_col]] %in% input$filtro_tarea_z_comp else TRUE
      )
    
    # Filtro de duración
    if (!is.null(input$filtro_duracion_input_z_comp)) {
      dur <- NULL
      if (!is.null(input$duration_col) && input$duration_col != "None" && input$duration_col %in% names(data_full)) {
        dur <- suppressWarnings(as.numeric(data_full[[input$duration_col]]))
      } else if (!is.null(input$start_col) && !is.null(input$end_col) &&
                 input$start_col %in% names(data_full) && input$end_col %in% names(data_full)) {
        hora_inicio <- suppressWarnings(parse_time(data_full[[input$start_col]]))
        hora_fin <- suppressWarnings(parse_time(data_full[[input$end_col]]))
        dur <- as.numeric(difftime(hora_fin, hora_inicio, units = "mins"))
      }
      if (!is.null(dur)) {
        keep <- !is.na(dur) & dur >= input$filtro_duracion_input_z_comp[1] & dur <= input$filtro_duracion_input_z_comp[2]
        data_full <- data_full[keep, ]
      }
    }
    
    # Fecha de la sesión actual
    sesion_fecha <- parse_date_time(input$filtro_sesion_selector_comp, orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
    
    # Procesar cada métrica
    resumen_list <- lapply(metrics, function(metric) {
      df_metric <- data_full
      
      # Filtro específico por valores
      filtro_id <- paste0("filtro_metrica_valor_z_comp_", make.names(metric))
      if (!is.null(input[[filtro_id]]) && metric %in% names(df_metric)) {
        vals <- suppressWarnings(as.numeric(df_metric[[metric]]))
        rango <- input[[filtro_id]]
        df_metric <- df_metric[!is.na(vals) & vals >= rango[1] & vals <= rango[2], ]
      }
      
      # Rolling stats previas
      stats_movil <- df_metric %>%
        filter(.data[[date_col]] < sesion_fecha) %>%
        arrange(.data[[player_col]], .data[[date_col]]) %>%
        group_by(Jugador = .data[[player_col]]) %>%
        reframe(
          Promedio_Movil = if (n() >= window_size) mean(tail(.data[[metric]], window_size), na.rm = TRUE) else NA_real_,
          SD_Movil = if (n() >= window_size) sd(tail(.data[[metric]], window_size), na.rm = TRUE) else NA_real_
        )
      
      # Valores del partido actual
      data_sesion <- df_metric %>%
        filter(as.character(.data[[date_col]]) == input$filtro_sesion_selector_comp) %>%
        mutate(Jugador = .data[[player_col]]) %>%
        select(Jugador, Fecha = .data[[date_col]], Valor = .data[[metric]])
      
      # Calcular z-score
      resumen <- left_join(data_sesion, stats_movil, by = "Jugador") %>%
        mutate(
          Métrica = metric,
          Z_score = (Valor - Promedio_Movil) / SD_Movil
        ) %>%
        filter(!is.na(Z_score) & is.finite(Z_score)) %>%
        mutate(
          Valor = round(Valor, 1),
          Promedio_Movil = round(Promedio_Movil, 1),
          SD_Movil = round(SD_Movil, 1),
          Z_score = round(Z_score, 1)
        ) %>%
        select(Métrica, Jugador, Fecha, Valor, Promedio_Movil, SD_Movil, Z_score)
      
      return(resumen)
    })
    
    # Unir y validar
    resumen_final <- bind_rows(resumen_list)
    
    if (nrow(resumen_final) == 0 || all(is.na(resumen_final$Z_score))) {
      return(DT::datatable(data.frame(
        Métrica = "Sin datos",
        Jugador = NA,
        Fecha = as.character(input$filtro_sesion_selector_comp),
        Valor = NA,
        Promedio_Movil = NA,
        SD_Movil = NA,
        Z_score = NA
      ), options = list(dom = 't', paging = FALSE)))
    }
    
    # Tabla final
    datatable(
      resumen_final,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'tip',
        class = 'cell-border stripe hover compact',
        autoWidth = TRUE
      ),
      rownames = FALSE,
      class = 'display nowrap cell-border compact stripe'
    ) %>%
      # 🎨 Estilo general de celdas
      formatStyle(
        columns = names(resumen_final),
        backgroundColor = '#1e1e1e',
        color = '#ffffff',
        fontFamily = 'Open Sans',
        fontSize = '14px'
      ) %>%
      # 🎯 Z-score coloreado
      formatStyle(
        'Z_score',
        backgroundColor = styleInterval(
          c(-1.5, 1.5),
          c('#2ecc71', '#f1c40f', '#e74c3c')
        ),
        color = 'black',
        fontWeight = 'bold'
      )
  })
  
  # 🔵 Output: Gráfico de ACWR Exponencial
  observe({
    req(input$metric_acwr, input$acwr_agudo_dias, input$acwr_cronico_dias)
    
    for (metrica in input$metric_acwr) {
      local({
        metrica_local <- metrica
        metrica_clean <- make.names(metrica_local)
        plot_id <- paste0("acwr_plot_", metrica_clean)
        filtro_id <- paste0("filtro_metrica_valor_acwr_", metrica_clean)
        
        output[[plot_id]] <- renderPlotly({
          req(read_data(), input$player_col, input$date_col, input[[filtro_id]])
          data <- read_data()
          
          # ==========================
          # 🔹 Aplicar filtros
          # ==========================
          if (!is.null(input$player_col) && input$player_col %in% names(data) && !is.null(input$filtro_jugador_acwr)) {
            data <- data[data[[input$player_col]] %in% input$filtro_jugador_acwr, ]
          }
          if (!is.null(input$position_col) && input$position_col %in% names(data) && !is.null(input$filtro_puesto_acwr)) {
            data <- data[data[[input$position_col]] %in% input$filtro_puesto_acwr, ]
          }
          if (!is.null(input$matchday_col) && input$matchday_col %in% names(data) && !is.null(input$filtro_matchday_acwr)) {
            data <- data[data[[input$matchday_col]] %in% input$filtro_matchday_acwr, ]
          }
          if (!is.null(input$task_col) && input$task_col %in% names(data) && !is.null(input$filtro_tarea_acwr)) {
            data <- data[data[[input$task_col]] %in% input$filtro_tarea_acwr, ]
          }
          if (!is.null(input$date_col) && input$date_col %in% names(data) && !is.null(input$filtro_fecha_acwr)) {
            fechas <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y")))
            data[[input$date_col]] <- as.Date(fechas)
            data <- data[!is.na(data[[input$date_col]]) & data[[input$date_col]] >= input$filtro_fecha_acwr[1] & data[[input$date_col]] <= input$filtro_fecha_acwr[2], ]
          }
          if (!is.null(input$filtro_duracion_input_acwr)) {
            dur <- NULL
            if (!is.null(input$duration_col) && input$duration_col != "None" && input$duration_col %in% names(data)) {
              dur <- suppressWarnings(as.numeric(data[[input$duration_col]]))
            } else if (!is.null(input$start_col) && input$start_col != "None" &&
                       !is.null(input$end_col) && input$end_col != "None" &&
                       input$start_col %in% names(data) && input$end_col %in% names(data)) {
              hora_inicio <- suppressWarnings(parse_time(data[[input$start_col]]))
              hora_fin <- suppressWarnings(parse_time(data[[input$end_col]]))
              dur <- as.numeric(difftime(hora_fin, hora_inicio, units = "mins"))
            }
            if (!is.null(dur)) {
              keep <- !is.na(dur) & dur >= input$filtro_duracion_input_acwr[1] & dur <= input$filtro_duracion_input_acwr[2]
              data <- data[keep, ]
            }
          }
          
          req(metrica_local %in% names(data))
          data[[metrica_local]] <- suppressWarnings(as.numeric(data[[metrica_local]]))
          val_range <- input[[filtro_id]]
          data <- data %>% filter(between(.data[[metrica_local]], val_range[1], val_range[2]))
          
          dias_agudo <- input$acwr_agudo_dias
          dias_cronico <- input$acwr_cronico_dias
          
          lambda_agudo <- log(2) / dias_agudo
          lambda_cronico <- log(2) / dias_cronico
          
          acwr_data <- data %>%
            arrange(.data[[input$player_col]], .data[[input$date_col]]) %>%
            group_by(Jugador = .data[[input$player_col]]) %>%
            mutate(
              EWMA_agudo = as.numeric(stats::filter(.data[[metrica_local]], lambda_agudo, method = "recursive")),
              EWMA_cronico = as.numeric(stats::filter(.data[[metrica_local]], lambda_cronico, method = "recursive")),
              ACWR = EWMA_agudo / EWMA_cronico,
              Fecha = as.Date(.data[[input$date_col]])
            ) %>%
            ungroup() %>%
            filter(!is.na(ACWR), is.finite(ACWR)) %>%
            group_by(Jugador, Fecha) %>%
            summarise(
              ACWR = mean(ACWR, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            mutate(
              tooltip = paste0("Jugador: ", Jugador, "<br>Fecha: ", Fecha, "<br>ACWR: ", round(ACWR, 2)),
              color = ifelse(ACWR > 1.5, "#fd002b", "#c8c8c8")
            )
          
          if (nrow(acwr_data) == 0) return(NULL)
          
          # Fondos de zonas de riesgo
          min_fecha <- min(acwr_data$Fecha, na.rm = TRUE)
          max_fecha <- max(acwr_data$Fecha, na.rm = TRUE)
          
          zonas_riesgo <- tibble::tibble(
            xmin = c(min_fecha, min_fecha),
            xmax = c(max_fecha, max_fecha),
            ymin = c(-Inf, 0.8),
            ymax = c(1.5, Inf),
            fill = c("#eafaf1", "#fdecea"),
            alpha = c(0.4, 0.4)
          )
          
          # Gráfico
          p <- ggplot(acwr_data, aes(x = Fecha, y = ACWR, text = tooltip)) +
            
            # Zonas de riesgo
            geom_rect(
              data = zonas_riesgo,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, alpha = alpha),
              inherit.aes = FALSE,
              show.legend = FALSE
            ) +
            
            # Barras
            geom_col(aes(fill = color), width = 1, show.legend = FALSE) +
            
            # Línea suavizada
            geom_smooth(aes(group = Jugador), method = "loess", span = 0.8, se = FALSE, color = "#ffffff", size = 0.8) +
            
            # Línea horizontal en ACWR = 1
            geom_hline(yintercept = 1, linetype = "dashed", color = "#ffffff", linewidth = 1.2) +
            
            facet_wrap(~Jugador, ncol = 4, scales = "fixed") +
            scale_fill_identity() +
            scale_alpha_identity() +
            expand_limits(y = 0) +
            theme_minimal(base_size = 14) +
            labs(
              title = paste("ACWR Exponencial –", metrica_local),
              x = "Fecha", y = "ACWR"
            ) +
            theme(
              plot.background = element_rect(fill = "#1e1e1e", color = NA),
              panel.background = element_rect(fill = "#1e1e1e", color = NA),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(color = "#2c2c2c"),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "#ffffff"),
              axis.text.y = element_text(size = 11, color = "#ffffff"),
              axis.title = element_text(face = "bold", size = 14, color = "#ffffff"),
              plot.title = element_text(
                hjust = 0.5, face = "bold", size = 20, color = "#fd002b",
                family = "Righteous"
              ),
              strip.text = element_text(size = 12, face = "bold", color = "#ffffff"),
              strip.background = element_blank(),
              legend.position = "none"
            )
          
          ggplotly(p, tooltip = "text") %>%
            layout(
              plot_bgcolor = "#1e1e1e",
              paper_bgcolor = "#1e1e1e",
              font = list(color = "#ffffff")
            )
        })
      })
    }
  })
}


shinyApp(ui, server)

