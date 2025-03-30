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
                   column(6,
                          uiOutput("filtro_jugador"),
                          uiOutput("filtro_puesto"),
                          uiOutput("filtro_matchday")
                   ),
                   column(6,
                          uiOutput("filtro_tarea"),
                          uiOutput("filtro_fecha"),
                          uiOutput("filtro_duracion")
                   )
                 ),
                 fluidRow(
                   column(6, selectInput("metrica_seleccionada", "Select Metric:", choices = NULL)),
                   column(6, uiOutput("filtro_valor_metrica"))
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
    numeric_cols <- input$metric_col[sapply(data[input$metric_col], is.numeric)]
    updateSelectInput(session, "metrica_seleccionada", choices = numeric_cols, selected = numeric_cols[1])
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
  
  output$filtro_jugador <- renderUI({
    req(read_data(), input$player_col)
    selectInput("filtro_jugador_input", "Jugador/es:", choices = unique(read_data()[[input$player_col]]), multiple = TRUE)
  })
  
  output$filtro_puesto <- renderUI({
    req(read_data(), input$position_col)
    selectInput("filtro_puesto_input", "Puesto/s:", choices = unique(read_data()[[input$position_col]]), multiple = TRUE)
  })
  
  output$filtro_matchday <- renderUI({
    req(read_data(), input$matchday_col)
    selectInput("filtro_matchday_input", "Match Day:", choices = unique(read_data()[[input$matchday_col]]), multiple = TRUE)
  })
  
  output$filtro_tarea <- renderUI({
    req(read_data(), input$task_col)
    selectInput("filtro_tarea_input", "Tarea/s:", choices = unique(read_data()[[input$task_col]]), multiple = TRUE)
  })
  
  output$filtro_fecha <- renderUI({
    req(read_data(), input$date_col)
    fechas <- parse_date_time(read_data()[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
    fechas <- fechas[!is.na(fechas)]
    if (length(fechas) == 0) return(NULL)
    dateRangeInput("filtro_fecha_input", "Rango de Fechas:",
                   start = min(fechas), end = max(fechas))
  })
  
  output$filtro_duracion <- renderUI({
    dur <- session_duration()
    dur <- dur[!is.na(dur) & is.finite(dur)]
    if (length(dur) == 0) return(NULL)
    sliderInput("filtro_duracion_input", "Duración (minutos):",
                min = floor(min(dur)), max = ceiling(max(dur)),
                value = c(floor(min(dur)), ceiling(max(dur))))
  })
  
  output$filtro_valor_metrica <- renderUI({
    req(read_data(), input$metrica_seleccionada)
    valores <- suppressWarnings(as.numeric(read_data()[[input$metrica_seleccionada]]))
    valores <- valores[!is.na(valores) & is.finite(valores)]
    if (length(valores) == 0) return(NULL)
    sliderInput("filtro_valor_metrica_input", paste("Rango de", input$metrica_seleccionada),
                min = floor(min(valores)), max = ceiling(max(valores)),
                value = c(floor(min(valores)), ceiling(max(valores))))
  })
  
  filtered_data <- reactive({
    req(read_data(), input$player_col, input$position_col, input$matchday_col,
        input$task_col, input$date_col, input$metrica_seleccionada)
    
    data <- read_data()
    
    if (!is.null(input$filtro_jugador_input)) {
      data <- data[data[[input$player_col]] %in% input$filtro_jugador_input, ]
    }
    if (!is.null(input$filtro_puesto_input)) {
      data <- data[data[[input$position_col]] %in% input$filtro_puesto_input, ]
    }
    if (!is.null(input$filtro_matchday_input)) {
      data <- data[data[[input$matchday_col]] %in% input$filtro_matchday_input, ]
    }
    if (!is.null(input$filtro_tarea_input)) {
      data <- data[data[[input$task_col]] %in% input$filtro_tarea_input, ]
    }
    if (!is.null(input$filtro_fecha_input)) {
      fechas <- parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
      data <- data[fechas >= input$filtro_fecha_input[1] & fechas <= input$filtro_fecha_input[2], ]
    }
    if (!is.null(input$filtro_duracion_input)) {
      dur <- session_duration()
      dur <- dur[seq_len(nrow(data))]  # Match length
      keep <- !is.na(dur) & dur >= input$filtro_duracion_input[1] & dur <= input$filtro_duracion_input[2]
      data <- data[keep, ]
    }
    if (!is.null(input$filtro_valor_metrica_input)) {
      valores <- suppressWarnings(as.numeric(data[[input$metrica_seleccionada]]))
      keep <- !is.na(valores) & valores >= input$filtro_valor_metrica_input[1] & valores <= input$filtro_valor_metrica_input[2]
      data <- data[keep, ]
    }
    
    return(data)
  })
  
  output$barras_fecha_plot <- renderPlotly({
    req(filtered_data(), input$player_col, input$date_col, input$metrica_seleccionada)
    data <- filtered_data()
    
    data[[input$metrica_seleccionada]] <- suppressWarnings(as.numeric(data[[input$metrica_seleccionada]]))
    data[[input$date_col]] <- parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
    data <- data[!is.na(data[[input$date_col]]), ]
    
    plot_data <- data %>%
      group_by(Fecha = .data[[input$date_col]], Jugador = .data[[input$player_col]]) %>%
      summarise(Promedio = mean(.data[[input$metrica_seleccionada]], na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(plot_data, aes(x = Fecha, y = Promedio, fill = Jugador)) +
      geom_col(position = "dodge") +
      theme_minimal(base_size = 14) +
      labs(title = paste("Promedio de", input$metrica_seleccionada, "por Fecha y Jugador"),
           x = "Fecha", y = input$metrica_seleccionada) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$table <- renderDT({
    req(filtered_data())
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  output$summary_table <- renderDT({
    req(filtered_data(), input$metrica_seleccionada)
    data <- filtered_data()
    valores <- suppressWarnings(as.numeric(data[[input$metrica_seleccionada]]))
    resumen <- data.frame(
      Mean = mean(valores, na.rm = TRUE),
      Min = min(valores, na.rm = TRUE),
      Max = max(valores, na.rm = TRUE),
      SD = sd(valores, na.rm = TRUE)
    )
    datatable(resumen)
  })
}

shinyApp(ui, server)
