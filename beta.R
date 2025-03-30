library(shiny)
library(readr)
library(readxl)
library(jsonlite)
library(DT)
library(dplyr)
library(plotly)
library(lubridate)

# UI
ui <- fluidPage(
  titlePanel("GPS Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Subí tu archivo GPS", accept = c(".csv", ".xlsx", ".json")),
      tags$hr(),
      uiOutput("column_mapping"),
      uiOutput("player_filter"),
      uiOutput("position_filter"),
      uiOutput("matchday_filter"),
      uiOutput("task_filter"),
      uiOutput("date_filter"),
      selectInput("selected_metric", "Métrica a visualizar", choices = NULL),
      uiOutput("metric_value_filters"),
      uiOutput("duration_filter")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tabla", DTOutput("table")),
        tabPanel("Resumen", DTOutput("summary_table")),
        tabPanel("Visualización", plotlyOutput("metric_plot"))
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  read_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    file <- input$file$datapath
    
    data <- switch(ext,
                   "csv" = read_csv(file, show_col_types = FALSE),
                   "xlsx" = read_excel(file),
                   "json" = fromJSON(file, flatten = TRUE),
                   stop("Formato no soportado."))
    
    colnames(data) <- make.names(colnames(data))
    data
  })
  
  output$column_mapping <- renderUI({
    req(read_data())
    cols <- names(read_data())
    tagList(
      selectInput("player_col", "Columna: Jugador", choices = cols),
      selectInput("position_col", "Columna: Puesto", choices = cols),
      selectInput("matchday_col", "Columna: Match Day", choices = cols),
      selectInput("task_col", "Columna: Tarea", choices = cols),
      selectInput("date_col", "Columna: Fecha", choices = cols),
      selectInput("metric_col", "Columnas: Métricas", choices = cols, multiple = TRUE),
      selectInput("duration_col", "Columna de Duración (opcional):", choices = cols),
      selectInput("start_col", "Hora de Inicio (si no hay duración):", choices = cols),
      selectInput("end_col", "Hora de Fin (si no hay duración):", choices = cols)
    )
  })
  
  observeEvent(input$metric_col, {
    updateSelectInput(session, "selected_metric",
                      choices = input$metric_col,
                      selected = input$metric_col[1])
  })
  
  observeEvent(input$player_col, {
    req(read_data())
    updateSelectInput(session, "selected_player", choices = unique(read_data()[[input$player_col]]))
  })
  observeEvent(input$position_col, {
    req(read_data())
    updateSelectInput(session, "selected_position", choices = unique(read_data()[[input$position_col]]))
  })
  observeEvent(input$matchday_col, {
    req(read_data())
    updateSelectInput(session, "selected_matchday", choices = unique(read_data()[[input$matchday_col]]))
  })
  observeEvent(input$task_col, {
    req(read_data())
    updateSelectInput(session, "selected_task", choices = unique(read_data()[[input$task_col]]))
  })
  
  output$player_filter <- renderUI({ req(input$player_col); selectInput("selected_player", "Filtrar por Jugador", choices = NULL, multiple = TRUE) })
  output$position_filter <- renderUI({ req(input$position_col); selectInput("selected_position", "Filtrar por Puesto", choices = NULL, multiple = TRUE) })
  output$matchday_filter <- renderUI({ req(input$matchday_col); selectInput("selected_matchday", "Filtrar por Match Day", choices = NULL, multiple = TRUE) })
  output$task_filter <- renderUI({ req(input$task_col); selectInput("selected_task", "Filtrar por Tarea", choices = NULL, multiple = TRUE) })
  
  output$date_filter <- renderUI({
    req(read_data(), input$date_col)
    fechas <- suppressWarnings(parse_date_time(read_data()[[input$date_col]], orders = c("ymd", "dmy", "mdy")))
    fechas <- as.Date(fechas)
    if (all(is.na(fechas))) return(tags$span("⚠️ No se detectaron fechas válidas."))
    dateRangeInput("selected_date", "Filtrar por Fecha",
                   start = min(fechas, na.rm = TRUE),
                   end = max(fechas, na.rm = TRUE),
                   format = "dd-mm-yyyy")
  })
  
  output$metric_value_filters <- renderUI({
    req(read_data(), input$metric_col)
    data <- read_data()
    sliders <- lapply(input$metric_col, function(metric) {
      valores <- suppressWarnings(as.numeric(data[[metric]]))
      valores <- valores[!is.na(valores)]
      if (length(valores) == 0) return(NULL)
      sliderInput(paste0("range_", metric), paste("Filtrar", metric),
                  min = floor(min(valores)), max = ceiling(max(valores)),
                  value = c(floor(min(valores)), ceiling(max(valores))), step = 1)
    })
    tagList(sliders)
  })
  
  session_duration <- reactive({
    req(read_data())
    data <- read_data()
    
    if (!is.null(input$duration_col) && input$duration_col %in% names(data)) {
      return(suppressWarnings(as.numeric(data[[input$duration_col]]) * 60))  # Convertir a minutos
    }
    
    if (!is.null(input$start_col) && !is.null(input$end_col)) {
      start <- suppressWarnings(parse_date_time(data[[input$start_col]], orders = c("HMS", "HM", "ymd HMS", "ymd HM")))
      end <- suppressWarnings(parse_date_time(data[[input$end_col]], orders = c("HMS", "HM", "ymd HMS", "ymd HM")))
      return(as.numeric(difftime(end, start, units = "mins")))
    }
    return(NULL)
  })
  
  output$duration_filter <- renderUI({
    duracion <- session_duration()
    if (is.null(duracion) || all(is.na(duracion))) return(NULL)
    duracion <- duracion[!is.na(duracion)]
    sliderInput("duration_range", "Filtrar por duración (minutos)",
                min = floor(min(duracion)), max = ceiling(max(duracion)),
                value = c(floor(min(duracion)), ceiling(max(duracion))), step = 1)
  })
  
  processed_data <- reactive({
    req(read_data(), input$metric_col)
    data <- read_data()
    
    if (!is.null(input$selected_player)) data <- data[data[[input$player_col]] %in% input$selected_player, ]
    if (!is.null(input$selected_position)) data <- data[data[[input$position_col]] %in% input$selected_position, ]
    if (!is.null(input$selected_matchday)) data <- data[data[[input$matchday_col]] %in% input$selected_matchday, ]
    if (!is.null(input$selected_task)) data <- data[data[[input$task_col]] %in% input$selected_task, ]
    
    if (!is.null(input$selected_date)) {
      fechas <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("ymd", "dmy", "mdy")))
      data[[input$date_col]] <- as.Date(fechas)
      data <- data[data[[input$date_col]] >= input$selected_date[1] & data[[input$date_col]] <= input$selected_date[2], ]
    }
    
    for (metric in input$metric_col) {
      id <- paste0("range_", metric)
      if (!is.null(input[[id]])) {
        data[[metric]] <- suppressWarnings(as.numeric(data[[metric]]))
        data <- data[data[[metric]] >= input[[id]][1] & data[[metric]] <= input[[id]][2], ]
      }
    }
    
    if (!is.null(input$duration_range)) {
      duracion <- session_duration()
      if (!is.null(duracion)) {
        data <- data[duracion >= input$duration_range[1] & duracion <= input$duration_range[2], ]
      }
    }
    
    data
  })
  
  output$table <- renderDT({
    req(processed_data())
    datatable(processed_data(), options = list(pageLength = 10))
  })
  
  output$summary_table <- renderDT({
    req(processed_data(), input$metric_col)
    data <- processed_data()
    resumen <- lapply(input$metric_col, function(metric) {
      valores <- suppressWarnings(as.numeric(data[[metric]]))
      data.frame(
        Métrica = metric,
        Media = round(mean(valores, na.rm = TRUE), 2),
        Mínimo = round(min(valores, na.rm = TRUE), 2),
        Máximo = round(max(valores, na.rm = TRUE), 2),
        SD = round(sd(valores, na.rm = TRUE), 2)
      )
    })
    datatable(do.call(rbind, resumen), options = list(pageLength = 10))
  })
  
  output$metric_plot <- renderPlotly({
    req(processed_data(), input$selected_metric)
    data <- processed_data()
    
    fechas <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("ymd", "dmy", "mdy")))
    data[[input$date_col]] <- as.Date(fechas)
    data[[input$selected_metric]] <- suppressWarnings(as.numeric(data[[input$selected_metric]]))
    
    plot_data <- data %>%
      group_by(Player = .data[[input$player_col]], Fecha = .data[[input$date_col]]) %>%
      summarise(Media = mean(.data[[input$selected_metric]], na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(plot_data, aes(x = Fecha, y = Media, fill = Player)) +
      geom_col(position = "dodge") +
      labs(title = paste("Evolución de", input$selected_metric), x = "Fecha", y = "Valor promedio") +
      theme_minimal(base_size = 14)
    
    ggplotly(p)
  })
}

shinyApp(ui, server)

