library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("Debug Microciclo"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Cargar CSV"),
      uiOutput("filtros_ui"),
      sliderInput("ventana", "Ventana MD:", 3, 10, 5),
      uiOutput("fechas_ui"),
      selectInput("metrica", "Métrica:", choices = NULL)
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  data_raw <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    df
  })
  
  output$filtros_ui <- renderUI({
    req(data_raw())
    data <- data_raw()
    cols <- names(data)
    tagList(
      selectInput("jugador_col", "Columna Jugador:", choices = cols),
      selectInput("fecha_col", "Columna Fecha:", choices = cols),
      selectInput("match_col", "Columna MatchDay:", choices = cols)
    )
  })
  
  output$fechas_ui <- renderUI({
    req(data_raw(), input$fecha_col, input$match_col)
    data <- data_raw()
    data[[input$fecha_col]] <- parse_date_time(data[[input$fecha_col]], orders = c("ymd", "dmy", "mdy"))
    data$tipo <- ifelse(grepl("MD", toupper(data[[input$match_col]])), "partido", "entreno")
    fechas_entreno <- data %>%
      filter(tipo == "entreno") %>%
      distinct(fecha = as.Date(.data[[input$fecha_col]])) %>%
      arrange(desc(fecha)) %>%
      pull(fecha)
    
    checkboxGroupInput("fechas", "Fechas entreno:", choices = fechas_entreno, selected = head(fechas_entreno, 3))
  })
  
  observe({
    req(data_raw())
    num_cols <- names(data_raw())[sapply(data_raw(), is.numeric)]
    updateSelectInput(session, "metrica", choices = num_cols)
  })
  
  output$plot <- renderPlotly({
    req(data_raw(), input$jugador_col, input$fecha_col, input$match_col, input$metrica, input$fechas)
    data <- data_raw()
    
    data[[input$fecha_col]] <- parse_date_time(data[[input$fecha_col]], orders = c("ymd", "dmy", "mdy"))
    data <- data[!is.na(data[[input$fecha_col]]), ]
    
    data$tipo <- ifelse(grepl("MD", toupper(data[[input$match_col]])), "partido", "entreno")
    
    partidos <- data %>%
      filter(tipo == "partido") %>%
      arrange(.data[[input$jugador_col]], desc(.data[[input$fecha_col]])) %>%
      group_by(Jugador = .data[[input$jugador_col]]) %>%
      slice_head(n = input$ventana) %>%
      summarise(partido = mean(.data[[input$metrica]], na.rm = TRUE), .groups = "drop")
    
    entrenos <- data %>%
      filter(tipo == "entreno", as.Date(.data[[input$fecha_col]]) %in% as.Date(input$fechas)) %>%
      group_by(Jugador = .data[[input$jugador_col]]) %>%
      summarise(entreno = sum(.data[[input$metrica]], na.rm = TRUE), .groups = "drop")
    
    df <- inner_join(partidos, entrenos, by = "Jugador") %>%
      filter(!is.na(partido), !is.na(entreno), entreno > 0) %>%
      mutate(
        ratio = partido / entreno,
        color = case_when(
          ratio > 1.2 ~ "#fd002b",
          ratio < 0.8 ~ "#00e676",
          TRUE ~ "#c8c8c8"
        )
      )
    
    if (nrow(df) == 0) return(NULL)
    
    p <- ggplot(df, aes(x = Jugador, y = ratio, fill = color, text = paste0("Jugador: ", Jugador, "<br>Ratio: ", round(ratio, 2)))) +
      geom_col() +
      geom_hline(yintercept = 1, linetype = "dashed", color = "white") +
      scale_fill_identity() +
      theme_minimal() +
      labs(title = "Ratio Partido / Entreno")
    
    ggplotly(p, tooltip = "text")
  })
  
}

shinyApp(ui, server)

