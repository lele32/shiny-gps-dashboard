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
library(rlang)
library(bs4Dash)
library(rsconnect)       # Deployment
library(slider)          # Rolling window stats
library(shinyWidgets)    # Custom inputs
library(tidyr)           # Data reshaping
library(fontawesome)     # íconos modernos
library(TTR)             # Para usar TTR::EMA
library(shinyalert)      # Para alertas tipo toast

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
  bg = "#0E1117",         # Fondo oscuro elegante
  fg = "#ffffff",         # Texto blanco
  primary = "#00FFFF",    # Acento: azul eléctrico (puede alternarse con verde menta o violeta)
  secondary = "#7F00FF",  # Acento secundario: violeta
  base_font = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Space Grotesk")
)

# ──────────────────────────────────────────────
# 🧩 Interfaz de usuario (UI)
# ──────────────────────────────────────────────
ui <- fluidPage(
  theme = tema_gps,
  
  # ===============================================================
  # ✨ ESTILOS PERSONALIZADOS (Actualizado para Shiny Selectize)
  # ===============================================================
  # Estilos personalizados y fuentes
  tags$head(
    # Bootstrap Icons (para íconos visuales)
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"),
    
    # Estilos personalizados
    tags$style(HTML("
      /* Tipografía base */
      body {
        font-family: 'Satoshi', sans-serif;
      }

      h1, h2, h3, h4, h5 {
        font-family: 'Geist', sans-serif;
      }

      /* Fondo blur estilo glassmorphism */
      .glass-box {
        background: rgba(255, 255, 255, 0.05);
        border: 1px solid rgba(0, 255, 255, 0.15);
        border-radius: 20px;
        backdrop-filter: blur(14px);
        padding: 20px;
        box-shadow: 0 0 12px rgba(0,255,255,0.04);
      }

    /* FILTROS */
    .filter-row {
      display: flex;
      flex-wrap: wrap;
      gap: 24px;
      margin-bottom: 24px;
    }
    .filter-column {
      flex: 1 1 45%;
    }

    /* CONTAINERS / INPUTS */
    .shiny-input-container {
      font-family: 'Satoshi', sans-serif;
      color: #ffffff;
    }
    select, input, textarea {
      background: rgba(255,255,255,0.05);
      border: 1px solid rgba(0,255,255,0.2);
      color: #ffffff;
      border-radius: 12px;
      backdrop-filter: blur(10px);
      font-family: 'Satoshi', sans-serif;
      transition: all 0.3s ease;
    }
    select:focus, input:focus, textarea:focus {
      outline: none;
      border-color: #00FFFF;
      box-shadow: 0 0 6px rgba(0,255,255,0.5);
    }

    /* SELECTIZE STYLE */
    .selectize-control .selectize-input {
      background: rgba(255,255,255,0.05) !important;
      border-radius: 12px !important;
      border: 1px solid rgba(0,255,255,0.2) !important;
      color: #ffffff !important;
      backdrop-filter: blur(10px);
    }

    .selectize-dropdown, .selectize-input.full, .selectize-dropdown .option {
      background: rgba(14,17,23,0.95) !important;
      color: #ffffff !important;
    }

    .selectize-dropdown .option:hover,
    .selectize-dropdown .option.active {
      background: #1f2937 !important;
      color: #00FFFF !important;
    }

    /* TABS estilo Baremetrics */
    .nav-tabs {
      background: rgba(255,255,255,0.02);
      border-bottom: 1px solid #00FFFF;
    }
    .nav-tabs > li > a {
      color: #ffffff;
      font-weight: bold;
      font-family: 'Satoshi', sans-serif;
      border: none;
      transition: all 0.3s ease;
    }
    .nav-tabs > li.active > a,
    .nav-tabs > li > a:hover {
      color: #00FFFF;
      border-bottom: 3px solid #00FFFF;
      background: transparent;
    }

    /* BOTONES GLASS + NEÓN */
    .btn {
      background: rgba(255,255,255,0.05);
      border: 1px solid rgba(0,255,255,0.3);
      color: #ffffff;
      border-radius: 12px;
      padding: 10px 20px;
      font-family: 'Satoshi', sans-serif;
      transition: all 0.3s ease;
    }
    .btn:hover {
      background: rgba(0,255,255,0.1);
      border-color: #00FFFF;
      color: #00FFFF;
      box-shadow: 0 0 10px rgba(0,255,255,0.3);
    }

    .btn-danger {
      background: #FF1744;
      border-color: #FF1744;
    }
    .btn-danger:hover {
      background: #ff4c68;
      border-color: #ff4c68;
      color: #ffffff;
    }

    /* DATATABLES HEADERS */
    table.dataTable thead th {
      background: #00FFFF !important;
      color: #0E1117 !important;
      font-family: 'Satoshi', sans-serif;
      font-weight: bold;
      font-size: 14px;
    }

    /* DATATABLES BODY */
    table.dataTable tbody td {
      background: #0E1117 !important;
      color: #ffffff !important;
      font-family: 'Satoshi', sans-serif;
    }

    /* FILAS hover */
    table.dataTable tbody tr:hover {
      background-color: rgba(0,255,255,0.08) !important;
    }
  "))
  ),
  
  # Encabezado principal estilo Hero minimalista
  tags$div(
    style = "
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    padding: 40px 20px;
    margin-bottom: 30px;
    background: rgba(255, 255, 255, 0.05);
    border: 1px solid rgba(0, 255, 255, 0.2);
    border-radius: 20px;
    backdrop-filter: blur(12px);
    box-shadow: 0 8px 30px rgba(0, 255, 255, 0.08);
  ",
    
    tags$img(
      src = "logo.png",
      height = "90px",
      style = "margin-bottom: 15px; filter: drop-shadow(0 0 8px rgba(0,255,255,0.3));"
    ),
    
    tags$h2("📊 GPS Data Dashboard")
  ),
  
  # Layout principal con sidebar
  fluidRow(
    # 🧭 Sidebar izquierdo
    column(
      width = 3,
      sidebarPanel(
        width = 12,  # Usa todo el ancho de la columna
        style = "
          background: rgba(255, 255, 255, 0.05);
          border: 1px solid rgba(0, 255, 255, 0.15);
          border-radius: 20px;
          backdrop-filter: blur(14px);
          padding: 20px;
          margin-top: 20px;
          box-shadow: 0 0 12px rgba(0,255,255,0.04);
        ",
        
        # Input Google Sheet
        textInput(
          "google_sheet_url", 
          label = tags$span(style = "color:#ffffff;", "Google Sheet URL or ID (optional):"), 
          value = ""
        ),
        
        # File Upload
        fileInput("file", "Upload GPS Data", 
                  multiple = TRUE, 
                  accept = c(".csv", ".xlsx", ".json"),
                  buttonLabel = "📁 Select Files...",
                  placeholder = "No file selected"),
        
        # Botón Reset
        actionButton("reset_base", "Reset DB", 
                     icon = icon("trash"),
                     class = "btn btn-danger",
                     style = "width:100%; margin-top:10px; margin-bottom:15px;"),
        
        # Estado DB
        uiOutput("estado_base"),
        
        tags$hr(style = "border-color: rgba(255,255,255,0.1);"),
        
        # Info del archivo
        uiOutput("file_info"),
        uiOutput("column_mapping")
      )
    ),
    
    column(
      width = 9,
      mainPanel(
        width = 12,
        style = "
          background: rgba(255, 255, 255, 0.03);
          border-radius: 20px;
          backdrop-filter: blur(12px);
          box-shadow: 0 0 12px rgba(255, 255, 255, 0.04);
          padding: 20px;
          margin-top: 20px;
        ",
        tabsetPanel(
          type = "tabs",
          id = "main_tabs",
          
          tabPanel(
            title = tagList(tags$i(class = "bi bi-table"), "Data Table"),
            DTOutput("table")
          ),
          
          #-------------------------------
          # 🟦 TAB PANEL: Time Series + KPIs
          #-------------------------------
          
          tabPanel(
            title = tagList(tags$i(class = "bi bi-graph-up"), "Time Series"),
            fluidRow(
              column(
                width = 4,
                class = "glass-box",
                lapply(c("jugador", "puesto", "matchday", "tarea", "fecha", "duracion"), function(id) {
                  tags$div(class = "filter-column", uiOutput(paste0("filtro_", id)))
                }),
                tags$div(class = "filter-column", selectInput("metric", "Select Metric:", choices = NULL, multiple = TRUE))
              ),
              column(
                width = 8,
                class = "glass-box",
                # 🔹 KPIs arriba del gráfico
                fluidRow(
                  style = "margin-bottom: 8px; margin-top: 0px; justify-content:center;",
                  uiOutput("kpi_row_time")
                ),
                uiOutput("barras_fecha_ui")
              )
            )
          ),
          
          #-------------------------------
          # 🟦 TAB PANEL: Boxplot MD
          #-------------------------------
          
          tabPanel(
            title = tagList(tags$i(class = "bi bi-box"), "Boxplot by Match Day"),
            fluidRow(
              column(
                width = 4,
                class = "glass-box",
                lapply(c("jugador_box", "puesto_box", "matchday_box", "tarea_box", "fecha_box", "duracion_box"), function(id) {
                  tags$div(class = "filter-column", uiOutput(paste0("filtro_", id)))
                }),
                tags$div(class = "filter-column", selectInput("metric_box", "Select Metrics:", choices = NULL, multiple = TRUE))
              ),
              column(
                width = 8,
                class = "glass-box",
                uiOutput("boxplot_matchday_ui")
              )
            )
          ),
          
          #-------------------------------
          # 🟦 TAB PANEL: Boxplot TAREA
          #-------------------------------
          
          
          tabPanel(
            title = tagList(tags$i(class = "bi bi-box-seam"), "Boxplot by Task"),
            fluidRow(
              column(
                width = 4,
                class = "glass-box",
                lapply(c("jugador_task", "puesto_task", "matchday_task", "tarea_task", "fecha_task", "duracion_task"), function(id) {
                  tags$div(class = "filter-column", uiOutput(paste0("filtro_", id)))
                }),
                tags$div(class = "filter-column", selectInput("metric_task", "Select Metrics:", choices = NULL, multiple = TRUE))
              ),
              column(
                width = 8,
                class = "glass-box",
                uiOutput("boxplot_task_ui")
              )
            )
          ),
          
          #-------------------------------
          # 🟦 TAB PANEL: Z score
          #-------------------------------
          
          
          tabPanel(
            title = tagList(tags$i(class = "bi bi-activity"), "Z-score Over Time"),
            fluidRow(
              column(
                width = 4,
                class = "glass-box",
                lapply(c("jugador_z", "puesto_z", "matchday_z", "tarea_z", "fecha_z", "duracion_z"), function(id) {
                  tags$div(class = "filter-column", uiOutput(paste0("filtro_", id)))
                }),
                tags$div(class = "filter-column", selectInput("metric_z", "Select Metric:", choices = NULL, multiple = TRUE))
              ),
              column(
                width = 8,
                class = "glass-box",
                uiOutput("zscore_plot_ui")
              )
            )
          ),
          
          #-------------------------------
          # 🟦 TAB PANEL: Análisis de sesión
          #-------------------------------
          
          tabPanel(
            title = tagList(tags$i(class = "bi bi-activity"), "Session Analysis"),
            fluidRow(
              column(
                width = 4,
                class = "glass-box",
                lapply(c("jugador_sesion", "puesto_sesion", "matchday_sesion", "tarea_sesion", "duracion_sesion"), function(id) {
                  tags$div(class = "filter-column", uiOutput(paste0("filtro_", id)))
                }),
                tags$div(class = "filter-column", uiOutput("filtro_sesion_selector")),
                tags$div(class = "filter-column", selectInput("metricas_sesion_plot", "Select Metrics:", choices = NULL, multiple = TRUE)),
                tags$div(class = "filter-column", uiOutput("filtro_fecha_sesion"))
              ),
              column(
                width = 8,
                class = "glass-box",
                uiOutput("graficos_metricas_sesion")
              )
            )
          ),
          
          #-------------------------------
          # 🟦 TAB PANEL: Análisis Competitivo
          #-------------------------------
          
          tabPanel(
            title = tagList(tags$i(class = "bi bi-trophy"), "Competitive Analysis"),
            fluidRow(
              column(
                width = 4,
                class = "glass-box",
                tags$div(class = "filter-column", uiOutput("filtro_jugador_z_comp")),
                tags$div(class = "filter-column", uiOutput("filtro_puesto_z_comp")),
                tags$div(class = "filter-column", uiOutput("filtro_tarea_z_comp")),
                tags$div(class = "filter-column", uiOutput("filtro_sesion_selector_comp")),
                tags$div(class = "filter-column", uiOutput("filtro_duracion_z_comp")),
                tags$div(class = "filter-column", selectInput("metric_z_comp", "Select Metrics:", choices = NULL, multiple = TRUE)),
                tags$div(class = "filter-column", sliderInput("ventana_movil_z_comp", "MD Movile Window:", min = 3, max = 5, value = 3, step = 1))
              ),
              column(
                width = 8,
                class = "glass-box",
                uiOutput("zscore_comp_plot_ui"),
                tags$hr(),
                DTOutput("tabla_resumen_comp")
              )
            )
          ),
          
          #-------------------------------
          # 🟦 TAB PANEL: ACWR
          #-------------------------------
          
          tabPanel(
            title = tagList(tags$i(class = "bi bi-lightning-charge"), "ACWR"),
            fluidRow(
              # Panel izquierdo: Filtros
              column(
                width = 4,
                class = "glass-box",
                tags$div(class = "filter-column", uiOutput("filtro_jugador_acwr")),
                tags$div(class = "filter-column", uiOutput("filtro_puesto_acwr")),
                tags$div(class = "filter-column", uiOutput("filtro_matchday_acwr")),
                tags$div(class = "filter-column", uiOutput("filtro_tarea_acwr")),
                tags$div(class = "filter-column", uiOutput("filtro_fecha_acwr")),
                tags$div(class = "filter-column", uiOutput("filtro_duracion_acwr")),
                tags$div(
                  class = "filter-column",
                  selectInput("metric_acwr", "Select Metrics:", choices = NULL, multiple = TRUE)
                )
              ),
              # Panel derecho: Sliders + gráfico
              column(
                width = 8,
                class = "glass-box",
                # ⬇️ Centrado horizontal y glass
                tags$div(
                  style = "display: flex; justify-content: center; align-items: flex-end; background: rgba(30,30,30,0.92); border-radius: 20px; padding: 14px 0 10px 0; margin-bottom: 16px; gap: 1.2em;",
                  # Slider Agudo
                  tags$div(
                    style = "background: rgba(14,17,23,0.92); border-radius: 16px; padding: 12px 14px 10px 16px; box-shadow: 0 2px 8px #10101040; min-width:220px; max-width:260px;",
                    tags$div(
                      style = "color:#00FFFF; font-weight:600; font-size:1.08em; margin-bottom:6px;",
                      tags$i(class = "bi bi-lightning-charge", style = "margin-right:6px; color:#fd002b; font-size:1.1em;"),
                      "Acute"
                    ),
                    sliderInput(
                      inputId = "acwr_agudo_dias",
                      label = tags$span("Acute (n Days)", style = "font-size:0.99em;"),
                      min = 3, max = 14, value = 7, step = 1, width = "100%"
                    )
                  ),
                  # Slider Crónico
                  tags$div(
                    style = "background: rgba(14,17,23,0.92); border-radius: 16px; padding: 12px 14px 10px 16px; box-shadow: 0 2px 8px #10101040; min-width:220px; max-width:260px;",
                    tags$div(
                      style = "color:#00FFFF; font-weight:600; font-size:1.08em; margin-bottom:6px;",
                      tags$i(class = "bi bi-lightning", style = "margin-right:6px; color:#fd002b; font-size:1.1em;"),
                      "Chronic"
                    ),
                    sliderInput(
                      inputId = "acwr_cronico_dias",
                      label = tags$span("Chronic (n Days)", style = "font-size:0.99em;"),
                      min = 14, max = 42, value = 28, step = 1, width = "100%"
                    )
                  )
                ),
                # 👇 Output dinámico para los gráficos
                uiOutput("acwr_plot_ui")
              )
            )
          ),
          
          #-------------------------------
          # 🟦 TAB PANEL: Gráfico de Análisis Microciclo
          #-------------------------------
          
          tabPanel(
            title = tagList(tags$i(class = "bi bi-bar-chart-line"), "Microcycle Analysis"),
            fluidRow(
              column(
                width = 4,
                class = "glass-box",
                tags$div(class = "filter-column", uiOutput("filtro_jugador_micro")),
                tags$div(class = "filter-column", uiOutput("filtro_puesto_micro")),
                tags$div(class = "filter-column", uiOutput("filtro_tarea_micro")),
                tags$div(class = "filter-column", uiOutput("filtro_duracion_micro")),
                tags$div(class = "filter-column", 
                         selectInput("metricas_microciclo", "Select Metrics:", choices = NULL, multiple = TRUE)
                ),
                tags$div(class = "filter-column", 
                         sliderInput(
                           inputId = "ventana_movil_micro",
                           label = tags$span(style = "color:#fd002b; font-weight:bold;", "MD Rolling Avg"),
                           min = 3, max = 10, value = 5, step = 1
                         )
                ),
                tags$div(class = "filter-column", uiOutput("selector_fechas_entreno_micro"))
              ),
              column(
                width = 8,
                class = "glass-box",
                # 👇 Bloque glass para umbrales + subtítulo
                tags$div(
                  style = "display: flex; flex-direction: column; gap: 0.6em; align-items: center; background: rgba(30,30,30,0.85); border-radius: 20px; padding: 14px 0 10px 0; margin-bottom: 14px;",
                  tags$p("Ajustá los umbrales de ratio por métrica:", style = "color: #ffffff; font-size: 1.12em; text-align: center; margin-bottom: 0.2em; letter-spacing: 0.5px;"),
                  uiOutput("umbral_ratio_microciclo_ui")  # Acá van los sliders por métrica
                ),
                uiOutput("microciclo_ratio_plot_ui")
              )
            )
          ),
          #-------------------------------
          # 🟦 TAB PANEL: Gráfico de Cuadrante (UI)
          #-------------------------------
          
          tabPanel(
            title = tagList(tags$i(class = "bi bi-grid-3x3-gap-fill"), "Quadrant"),
            fluidRow(
              # Filtros (columna izquierda)
              column(
                width = 4,
                class = "glass-box",
                # Filtro por Jugador
                tags$div(class = "filter-column", uiOutput("filtro_jugador_cuad")),
                # Filtro por Puesto
                tags$div(class = "filter-column", uiOutput("filtro_puesto_cuad")),
                # Filtro por MD
                tags$div(class = "filter-column", uiOutput("filtro_matchday_cuad")), 
                # Filtro por Tarea
                tags$div(class = "filter-column", uiOutput("filtro_tarea_cuad")),
                # Filtro por Duración
                tags$div(class = "filter-column", uiOutput("filtro_duracion_cuad")),
                # Filtro por Match Day / Sesión
                tags$div(class = "filter-column", uiOutput("filtro_sesion_cuad")),
                tags$div(
                  class = "filter-column",
                  selectInput(
                    inputId = "metricas_cuad",          # ESTE nombre debe coincidir en UI y observer
                    label = tags$span("Select Metrics:"),
                    choices = NULL,                          # Choices se cargan por observer
                    selected = NULL,
                    multiple = TRUE,
                  ),
                  uiOutput("warning_metricas_cuad")
                )
              ),
              #Panel de gráfico (columna derecha) con estética glass y sliders métricas
              column(
                width = 8,
                class = "glass-box",
                style = "border-radius: 24px; padding: 22px 16px 14px 16px;",
                tags$div(
                  style = "display: flex; flex-direction: column; gap: 0.6em; align-items: center; background: rgba(30,30,30,0.85); border-radius: 20px; padding: 14px 0 10px 0; margin-bottom: 14px;",
                  tags$p("Ajustá el rango de valores para cada métrica seleccionada:", 
                         style = "color: #ffffff; font-size: 1.12em; text-align: center; margin-bottom: 0.2em; letter-spacing: 0.5px;"),
                  uiOutput("sliders_metricas_cuad")  # Los sliders dinámicos por métrica
                ),
                uiOutput("cuadrante_plot_ui")    # El output Plotly va aquí
              )
            )
          )
        )
      )
    )  
  )
)


# =======================================================
# ⚙️ SERVER
# =======================================================
server <- function(input, output, session) {
  
  # ================================================================
  # 📦 BASE GLOBAL Y LECTURA DE ARCHIVOS / GOOGLE SHEETS
  # ================================================================
  
  base_datos_global <- reactiveVal(NULL)
  
  read_data <- reactive({
    req(base_datos_global())
    base_datos_global()
  })
  
  observeEvent({
    input$file
    input$google_sheet_url
  }, {
    
    google_sheet_url <- input$google_sheet_url
    file_input <- input$file
    
    data_new <- NULL
    
    if (nzchar(google_sheet_url)) {
      # 🔹 Leer de Google Sheets
      tryCatch({
        if (!grepl("^https?://", google_sheet_url)) {
          google_sheet_url <- paste0("https://docs.google.com/spreadsheets/d/", google_sheet_url, "/export?format=csv")
        } else {
          google_sheet_url <- sub("/edit.*", "/export?format=csv", google_sheet_url)
        }
        
        data_new <- readr::read_csv(google_sheet_url, show_col_types = FALSE)
        
        # 🔹 Toast SOLO si cargó bien
        shinyalert(
          title = "✅ Google Sheet Loaded",
          text = "Google Sheets data loaded successfully.",
          type = "success",
          timer = 2500,
          showConfirmButton = FALSE
        )
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error reading Google Sheets",
          paste("An error occurred:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      })
      
    } else if (!is.null(file_input)) {
      # 🔹 Leer múltiples archivos locales
      tryCatch({
        data_list <- lapply(seq_len(nrow(file_input)), function(i) {
          file_path <- file_input$datapath[i]
          ext <- tools::file_ext(file_input$name[i])
          
          switch(ext,
                 "csv" = {
                   first_line <- readLines(file_path, n = 1)
                   delim <- if (grepl(";", first_line)) ";" else ","
                   
                   raw_lines <- readLines(file_path, warn = FALSE)
                   header_line <- which(grepl('^(\"?Player Name\"?|\"?Username\"?)\\s*[,;]', raw_lines))[1]
                   
                   if (!is.na(header_line) && header_line > 1) {
                     readr::read_delim(file_path, delim = delim, skip = header_line - 1, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
                   } else {
                     readr::read_delim(file_path, delim = delim, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
                   }
                 },
                 "xlsx" = readxl::read_excel(file_path),
                 "json" = jsonlite::fromJSON(file_path, flatten = TRUE),
                 stop("Unsupported file type.")
          )
        })
        
        data_new <- bind_rows(data_list)
        
        # 🔹 Toast de carga exitosa de archivos (después de bind_rows)
        shinyalert(
          title = "✅ File(s) Uploaded",
          text = paste0(nrow(file_input), " file(s) uploaded successfully."),
          type = "success",
          timer = 2500,
          showConfirmButton = FALSE
        )
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error reading file(s)",
          paste("An error occurred:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      })
    }
    
    req(data_new)
    
    if (!is.data.frame(data_new)) {
      stop("The file does not contain a valid table format.")
    }
    
    colnames(data_new) <- make.names(colnames(data_new))
    
    # ========================================
    # 🔥 Agregar datos nuevos a la base global
    # ========================================
    
    if (is.null(base_datos_global())) {
      base_datos_global(data_new)
    } else if (is.data.frame(base_datos_global()) && is.data.frame(data_new)) {
      base_combinada <- bind_rows(base_datos_global(), data_new)
      
      # Evitar duplicados por Jugador + Fecha
      if (all(c("Jugador", "Fecha") %in% colnames(base_combinada))) {
        base_combinada <- base_combinada %>% distinct(Jugador, Fecha, .keep_all = TRUE)
      }
      
      base_datos_global(base_combinada)
    } else {
      showModal(modalDialog(
        title = "Error",
        "The file does not contain a valid table format.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    # ========================================
    # 🔥 Actualizar mapeo de columnas
    # ========================================
    
    update_mapped_columns <- function(cols) {
      guess_column <- function(possible_names) {
        match <- tolower(cols) %in% tolower(possible_names)
        if (any(match)) return(cols[which(match)[1]]) else return(NULL)
      }
      
      updateSelectInput(session, "player_col", selected = guess_column(c("player", "player name", "username", "jugador")))
      updateSelectInput(session, "position_col", selected = guess_column(c("position", "pos", "rol", "puesto")))
      updateSelectInput(session, "matchday_col", selected = guess_column(c("match day", "matchday", "match.day", "md", "dia", "día")))
      updateSelectInput(session, "task_col", selected = guess_column(c("task", "activity", "drill", "selection", "tarea")))
      updateSelectInput(session, "date_col", selected = guess_column(c("date", "fecha", "session date", "day")))
      updateSelectInput(session, "duration_col", selected = guess_column(c("duration", "duración", "duration_min")))
      updateSelectInput(session, "start_col", selected = guess_column(c("start", "inicio", "hora inicio", "start.hour")))
      updateSelectInput(session, "end_col", selected = guess_column(c("end", "fin", "hora fin", "end_time", "final.hour")))
      
      numeric_metrics <- cols[sapply(base_datos_global()[cols], is.numeric)]
      updateSelectInput(session, "metric_col", choices = numeric_metrics, selected = character(0))
    }
    
    update_mapped_columns(colnames(base_datos_global()))
  })
  
  # ================================================================
  # 📄 INFORMACIÓN DEL ARCHIVO
  # ================================================================
  
  output$file_info <- renderUI({
    req(input$file, input$google_sheet_url)
    
    # Si hay URL de Google Sheet pegada
    if (nzchar(input$google_sheet_url)) {
      tags$p(
        tags$b("File uploaded:"), "Google Sheets",
        tags$br(),
        tags$b("Link:"), a(input$google_sheet_url, href = input$google_sheet_url, target = "_blank")
      )
      
    } else if (!is.null(input$file)) {
      # Si se subieron múltiples archivos locales
      file_names <- input$file$name
      file_sizes <- input$file$size / 1024  # Convertir bytes a KB
      total_size <- sum(file_sizes, na.rm = TRUE)
      
      tagList(
        tags$p(tags$b("File uploaded:")),
        tags$ul(
          lapply(seq_along(file_names), function(i) {
            tags$li(paste0(file_names[i], " - ", round(file_sizes[i], 2), " KB"))
          })
        ),
        tags$p(tags$b("File Size:"), paste0(round(total_size, 2), " KB"))
      )
      
    } else {
      # No hay archivo cargado
      tags$p("No files uploaded.")
    }
  })
  
  # ================================================================
  # 🔍 MAPEOS DE COLUMNAS CLAVE
  # ================================================================
  
  output$column_mapping <- renderUI({
    req(base_datos_global())
    cols <- colnames(base_datos_global())
    
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
  
  # ================================================================
  # ♻️ RESET DE BASE DE DATOS GLOBAL
  # ================================================================
  
  observeEvent(input$reset_base, {
    showModal(modalDialog(
      title = "Confirm Reset",
      "Are you sure you want to clear the entire database and remove all uploaded files?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Reset", class = "btn btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirm_reset, {
    # 🔹 Resetear base global
    base_datos_global(NULL)
    
    # 🔹 Limpiar inputs de carga
    updateTextInput(session, "google_sheet_url", value = "")
    
    # 🔹 Limpiar mapeos
    updateSelectInput(session, "player_col", choices = character(0), selected = NULL)
    updateSelectInput(session, "position_col", choices = character(0), selected = NULL)
    updateSelectInput(session, "matchday_col", choices = character(0), selected = NULL)
    updateSelectInput(session, "task_col", choices = character(0), selected = NULL)
    updateSelectInput(session, "date_col", choices = character(0), selected = NULL)
    updateSelectInput(session, "duration_col", choices = character(0), selected = NULL)
    updateSelectInput(session, "start_col", choices = character(0), selected = NULL)
    updateSelectInput(session, "end_col", choices = character(0), selected = NULL)
    updateSelectInput(session, "metric_col", choices = character(0), selected = NULL)
    
    # 🔹 Cerrar modal de confirmación
    removeModal()
    
    # 🔥 Toast de éxito
    shinyalert(
      title = "✅ Database Reset",
      text = "You can start uploading new files.",
      type = "success",
      timer = 2500,  # milliseconds
      showConfirmButton = FALSE
    )
  })
  
  # ================================================================
  # 🧾 ESTADO DE LA BASE DE DATOS
  # ================================================================
  output$estado_base <- renderUI({
    data <- base_datos_global()
    
    if (is.null(data) || nrow(data) == 0) {
      tags$p("🗑️ Empty database. No data loaded.", style = "color: #fd002b; font-weight: bold;")
    } else {
      tags$p(paste0("✅ Database updated: ", nrow(data), " records"),
             style = "color: #00e676; font-weight: bold;")
    }
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
  
  #' 📌 Actualiza selectInput de métricas para el tab Microciclo
  observe({
    req(read_data(), input$metric_col)
    data <- read_data()
    selected_metrics <- input$metric_col
    valid_metrics <- selected_metrics[selected_metrics %in% colnames(data)]
    numeric_metrics <- valid_metrics[sapply(data[valid_metrics], is.numeric)]
    
    updateSelectInput(session, "metricas_microciclo",
                      choices = numeric_metrics,
                      selected = head(numeric_metrics, 1))
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
    output$filtro_jugador_sesion <- create_filter_ui("filtro_jugador_sesion", "player_col", "Filter by Player")
    output$filtro_jugador_z_comp <- create_filter_ui("filtro_jugador_z_comp", "player_col", "Filter by Player:")
    
    
    output$filtro_puesto       <- create_filter_ui("filtro_puesto", "position_col", "Filter by Position:")
    output$filtro_puesto_box   <- create_filter_ui("filtro_puesto_box", "position_col", "Filter by Position:")
    output$filtro_puesto_task  <- create_filter_ui("filtro_puesto_task", "position_col", "Filter by Position:")
    output$filtro_puesto_z     <- create_filter_ui("filtro_puesto_z", "position_col", "Filter by Position:")
    output$filtro_puesto_sesion  <- create_filter_ui("filtro_puesto_sesion", "position_col", "Filter by Position")
    output$filtro_puesto_z_comp  <- create_filter_ui("filtro_puesto_z_comp", "position_col", "Filter by Position:")
    
    
    output$filtro_matchday       <- create_filter_ui("filtro_matchday", "matchday_col", "Filter by Match Day:")
    output$filtro_matchday_box   <- create_filter_ui("filtro_matchday_box", "matchday_col", "Filter by Match Day:")
    output$filtro_matchday_task  <- create_filter_ui("filtro_matchday_task", "matchday_col", "Filter by Match Day:")
    output$filtro_matchday_z     <- create_filter_ui("filtro_matchday_z", "matchday_col", "Filter by Match Day:")
    output$filtro_matchday_sesion <- create_filter_ui("filtro_matchday_sesion", "matchday_col", "Filter by Match Day")
    
    
    output$filtro_tarea       <- create_filter_ui("filtro_tarea", "task_col", "Filter by Task:")
    output$filtro_tarea_box   <- create_filter_ui("filtro_tarea_box", "task_col", "Filter by Task:")
    output$filtro_tarea_task  <- create_filter_ui("filtro_tarea_task", "task_col", "Filter by Task:")
    output$filtro_tarea_z     <- create_filter_ui("filtro_tarea_z", "task_col", "Filter by Task:")
    output$filtro_tarea_sesion <- create_filter_ui("filtro_tarea_sesion", "task_col", "Filter by Task")
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
      label = "Select Sessions",
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
      label = "Select MD",
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
      "Filter by Player:", 
      choices = jugadores_unicos, 
      selected = default_selected,
      multiple = TRUE
    )
  })
  
  # 🔧 UI Outputs para filtros de Análisis de Microciclo
  
  output$filtro_jugador_micro <- renderUI({
    req(read_data(), input$player_col)
    data <- read_data()
    
    if (input$player_col %in% names(data)) {
      jugadores <- unique(data[[input$player_col]])
      selectInput("filtro_jugador_micro", "Filter by Player:", choices = jugadores, multiple = TRUE)
    } else {
      return(NULL)
    }
  })
  
  output$filtro_puesto_micro <- renderUI({
    req(read_data(), input$position_col)
    data <- read_data()
    
    if (input$position_col %in% names(data)) {
      puestos <- unique(data[[input$position_col]])
      selectInput("filtro_puesto_micro", "Filter by Position:", choices = puestos, multiple = TRUE)
    } else {
      return(NULL)
    }
  })
  
  output$filtro_tarea_micro <- renderUI({
    req(read_data(), input$task_col)
    data <- read_data()
    
    if (input$task_col %in% names(data)) {
      tareas <- unique(data[[input$task_col]])
      selectInput("filtro_tarea_micro", "Filter by Task:", choices = tareas, multiple = TRUE)
    } else {
      return(NULL)
    }
  })
  
  output$filtro_duracion_micro <- renderUI({
    req(read_data())
    data <- read_data()
    dur <- NULL
    
    # 🔹 Si hay columna directa de duración
    if (!is.null(input$duration_col) && input$duration_col != "None" && input$duration_col %in% names(data)) {
      dur <- suppressWarnings(as.numeric(data[[input$duration_col]]))
    } 
    
    # 🔹 Si hay columnas de hora inicio y fin
    else if (!is.null(input$start_col) && input$start_col != "None" &&
             !is.null(input$end_col) && input$end_col != "None" &&
             input$start_col %in% names(data) && input$end_col %in% names(data)) {
      hora_inicio <- suppressWarnings(parse_time(data[[input$start_col]]))
      hora_fin <- suppressWarnings(parse_time(data[[input$end_col]]))
      dur <- as.numeric(difftime(hora_fin, hora_inicio, units = "mins"))
    }
    
    # 🔹 Validar duración
    if (!is.null(dur)) {
      dur <- dur[!is.na(dur) & is.finite(dur) & dur > 0]
      
      if (length(dur) == 0) return(NULL)
      
      sliderInput(
        inputId = "filtro_duracion_micro",
        label = "Duration (min):",
        min = floor(min(dur)),
        max = ceiling(max(dur)),
        value = c(floor(min(dur)), ceiling(max(dur))),
        step = 1
      )
    } else {
      return(NULL)
    }
  })
  
  output$selector_fechas_entreno_micro <- renderUI({
    req(read_data(), input$date_col, input$matchday_col)
    data <- read_data()
    
    if (input$date_col %in% names(data) && input$matchday_col %in% names(data)) {
      fechas <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("ymd", "dmy", "mdy")))
      md <- as.character(data[[input$matchday_col]])
      
      fechas_entreno <- fechas[!is.na(fechas) & !is.na(md) & !grepl("^MD", md, ignore.case = TRUE)]
      fechas_unicas <- sort(unique(as.Date(fechas_entreno)))
      
      if (length(fechas_unicas) == 0) return(NULL)
      
      shinyWidgets::pickerInput(
        inputId = "fechas_entreno_micro",
        label = "Cumulative Sessions",
        choices = fechas_unicas,
        selected = tail(fechas_unicas, 3),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `selected-text-format` = "count > 2",
          `style` = "background-color: #1e1e1e; color: #ffffff;"
        )
      )
    } else {
      return(NULL)
    }
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
  
  #' 🔵 Reactive: Filtro para datos de Análisis de Microciclo
  #'
  #' Este bloque genera una base filtrada para comparar el acumulado de los últimos partidos 
  #' seleccionados mediante una ventana móvil (`ventana_movil_micro`) con el acumulado de días 
  #' específicos de semana seleccionados manualmente (`filtro_fechas_micro`).
  #'
  #' Filtros aplicados:
  #' - Jugador (`filtro_jugador_micro`)
  #' - Puesto (`filtro_puesto_micro`)
  #' - Tarea (`filtro_tarea_micro`)
  #' - Duración (`filtro_duracion_micro`)
  #' - Ventana móvil para partidos (`ventana_movil_micro`)
  #' - Fechas seleccionadas manualmente (`filtro_fechas_micro`)
  
  data_microciclo <- reactive({
    req(read_data(), input$metric_col, input$ventana_movil_micro, input$filtro_fechas_micro)
    
    data <- read_data()
    
    # Aplicar filtros básicos
    if (!is.null(input$player_col) && !is.null(input$filtro_jugador_micro)) {
      data <- data[data[[input$player_col]] %in% input$filtro_jugador_micro, ]
    }
    if (!is.null(input$position_col) && !is.null(input$filtro_puesto_micro)) {
      data <- data[data[[input$position_col]] %in% input$filtro_puesto_micro, ]
    }
    if (!is.null(input$task_col) && !is.null(input$filtro_tarea_micro)) {
      data <- data[data[[input$task_col]] %in% input$filtro_tarea_micro, ]
    }
    
    # Filtro por duración
    if (!is.null(input$filtro_duracion_micro)) {
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
        keep <- !is.na(dur) & dur >= input$filtro_duracion_micro[1] & dur <= input$filtro_duracion_micro[2]
        data <- data[keep, ]
      }
    }
    
    # Rolling de últimos partidos (matchday = "MD")
    data_matchday <- data %>% 
      filter(!is.null(input$matchday_col), .data[[input$matchday_col]] == "MD") %>%
      arrange(.data[[input$player_col]], desc(.data[[input$date_col]])) %>%
      group_by(.data[[input$player_col]]) %>%
      slice_head(n = input$ventana_movil_micro) %>%
      mutate(grupo = "Partidos") %>%
      ungroup()
    
    # Días de semana seleccionados manualmente
    fechas_semana <- input$filtro_fechas_micro
    data_semana <- data %>%
      filter(.data[[input$date_col]] %in% fechas_semana) %>%
      mutate(grupo = "Semana")
    
    # Combinar
    data_out <- bind_rows(data_matchday, data_semana)
    
    return(data_out)
  })
  
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
  
  # UI Dinámico para el checkbox de fechas (no-MD)
  output$filtro_fechas_micro <- renderUI({
    req(read_data(), input$date_col, input$matchday_col)
    data <- read_data()
    
    fechas_validas <- data %>%
      filter(!is.na(.data[[input$date_col]]) &
               !is.na(.data[[input$matchday_col]]) &
               !grepl("MD", .data[[input$matchday_col]], ignore.case = TRUE)) %>%
      distinct(fecha = .data[[input$date_col]]) %>%
      arrange(desc(fecha))
    
    checkboxGroupInput("fechas_micro", "Seleccionar Fechas para comparación:",
                       choices = fechas_validas$fecha,
                       selected = head(fechas_validas$fecha, 3))
  })
  
  # Datos filtrados para microciclo (matchday y comparación)
  filtro_data_micro <- reactive({
    req(read_data(), input$player_col, input$date_col, input$matchday_col, input$metric_col)
    
    data <- read_data()
    data[[input$date_col]] <- as.Date(data[[input$date_col]])
    
    # Aplicar filtros comunes
    if (!is.null(input$filtro_jugador_micro)) {
      data <- data[data[[input$player_col]] %in% input$filtro_jugador_micro, ]
    }
    if (!is.null(input$filtro_puesto_micro) && !is.null(input$position_col)) {
      data <- data[data[[input$position_col]] %in% input$filtro_puesto_micro, ]
    }
    if (!is.null(input$filtro_tarea_micro) && !is.null(input$task_col)) {
      data <- data[data[[input$task_col]] %in% input$filtro_tarea_micro, ]
    }
    if (!is.null(input$filtro_duracion_micro)) {
      dur <- NULL
      if (!is.null(input$duration_col) && input$duration_col != "None") {
        dur <- suppressWarnings(as.numeric(data[[input$duration_col]]))
      } else if (!is.null(input$start_col) && !is.null(input$end_col)) {
        hora_inicio <- suppressWarnings(parse_time(data[[input$start_col]]))
        hora_fin <- suppressWarnings(parse_time(data[[input$end_col]]))
        dur <- as.numeric(difftime(hora_fin, hora_inicio, units = "mins"))
      }
      if (!is.null(dur)) {
        data <- data[!is.na(dur) & dur >= input$filtro_duracion_micro[1] & dur <= input$filtro_duracion_micro[2], ]
      }
    }
    
    # Datos para el promedio móvil de últimos partidos
    data_md <- data %>%
      filter(grepl("MD", .data[[input$matchday_col]], ignore.case = TRUE)) %>%
      arrange(.data[[input$player_col]], desc(.data[[input$date_col]])) %>%
      group_by(Jugador = .data[[input$player_col]]) %>%
      slice_head(n = input$ventana_movil_micro) %>%
      mutate(grupo = "Partidos") %>%
      ungroup()
    
    # Datos para días seleccionados como "semana"
    data_ref <- data %>%
      filter(.data[[input$date_col]] %in% input$fechas_micro) %>%
      mutate(grupo = "Semana")
    
    bind_rows(data_md, data_ref)
  })
  
  ## UI Para selector de umbrales de ratio para analisis de microciclo 
  output$umbral_ratio_microciclo_ui <- renderUI({
    req(input$metricas_microciclo)
    n <- length(input$metricas_microciclo)
    if (n == 0) return(NULL)
    
    # Agrupa las métricas en filas de a 3
    filas <- split(input$metricas_microciclo, ceiling(seq_along(input$metricas_microciclo)/3))
    
    tagList(
      lapply(filas, function(met_group) {
        # El ancho de cada columna depende de la cantidad de métricas en la fila (máx 3 → 4 columnas de 12)
        ancho_col <- switch(length(met_group),
                            '1' = 12,  # Si solo hay una métrica en la fila, ocupa todo el ancho
                            '2' = 6,   # Si hay dos, 6 cada una
                            '3' = 4,   # Si hay tres, 4 cada una
                            4)         # Default 4 (por si acaso)
        fluidRow(
          lapply(met_group, function(metrica) {
            column(
              width = ancho_col, style = "padding-left: 8px; padding-right: 8px; margin-bottom: 6px;",
              tags$div(
                style = "background: rgba(14,17,23,0.92); border-radius: 16px; padding: 10px 10px 8px 12px; box-shadow: 0 2px 8px #10101040; min-width:200px;",
                tags$div(
                  style = "color:#00FFFF; font-weight:600; font-size:1.1em; margin-bottom:6px;",
                  metrica
                ),
                fluidRow(
                  column(
                    width = 6, style = "padding-right:6px;",
                    sliderInput(
                      inputId = paste0("umbral_bajo_", metrica),
                      label = tags$span("Bajo", style = "color:#00e676; font-size:0.95em;"),
                      min = 0, max = 2, value = 0.8, step = 0.01, width = "100%"
                    )
                  ),
                  column(
                    width = 6, style = "padding-left:6px;",
                    sliderInput(
                      inputId = paste0("umbral_alto_", metrica),
                      label = tags$span("Alto", style = "color:#fd002b; font-size:0.95em;"),
                      min = 0.8, max = 3, value = 1.5, step = 0.01, width = "100%"
                    )
                  )
                )
              )
            )
          })
        )
      })
    )
  })
  
  # 🧠 UI dinámico: Cuadrante
  
  #-------------------------------------------
  # FILTROS PARA EL CUADRANTE
  #-------------------------------------------
  
  # Filtro de Jugador
  output$filtro_jugador_cuad <- renderUI({
    req(read_data(), input$player_col)
    data <- read_data()
    if (input$player_col %in% names(data)) {
      jugadores <- unique(data[[input$player_col]])
      selectInput("filtro_jugador_cuad", "Filter by Player:", choices = jugadores, multiple = TRUE)
    } else {
      return(NULL)
    }
  })
  
  # Filtro de Puesto
  output$filtro_puesto_cuad <- renderUI({
    req(read_data(), input$position_col)
    data <- read_data()
    if (input$position_col %in% names(data)) {
      puestos <- unique(data[[input$position_col]])
      selectInput("filtro_puesto_cuad", "Filter by Position:", choices = puestos, multiple = TRUE)
    } else {
      return(NULL)
    }
  })
  
  # Filtro de MD
  output$filtro_matchday_cuad <- renderUI({
    req(read_data(), input$matchday_col)
    data <- read_data()
    if (input$matchday_col %in% names(data)) {
      matchdays <- unique(data[[input$matchday_col]])
      selectInput("filtro_matchday_cuad", "Filter by Match Day:", choices = matchdays, multiple = TRUE)
    } else {
      return(NULL)
    }
  })
  
  # Filtro de Tarea
  output$filtro_tarea_cuad <- renderUI({
    req(read_data(), input$task_col)
    data <- read_data()
    if (input$task_col %in% names(data)) {
      tareas <- unique(data[[input$task_col]])
      selectInput("filtro_tarea_cuad", "Filter by Task:", choices = tareas, multiple = TRUE)
    } else {
      return(NULL)
    }
  })
  
  # Filtro de Duración
  output$filtro_duracion_cuad <- renderUI({
    req(read_data())
    data <- read_data()
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
      dur <- dur[!is.na(dur) & is.finite(dur) & dur > 0]
      if (length(dur) == 0) return(NULL)
      sliderInput(
        inputId = "filtro_duracion_cuad",
        label = "Duration (min):",
        min = floor(min(dur)),
        max = ceiling(max(dur)),
        value = c(floor(min(dur)), ceiling(max(dur))),
        step = 1
      )
    } else {
      return(NULL)
    }
  })
  
  # Filtro de Sesión (selector de fecha para el cuadrante)
  output$filtro_sesion_cuad <- renderUI({
    req(read_data(), input$date_col)
    data <- read_data()
    # Aplica filtro de Match Day ANTES de obtener las fechas
    if (!is.null(input$matchday_col) && !is.null(input$filtro_matchday_cuad)) {
      if (input$matchday_col %in% names(data)) {
        data <- data[data[[input$matchday_col]] %in% input$filtro_matchday_cuad, ]
      }
    }
    
    # Parseo seguro de fechas
    fechas <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("ymd", "dmy", "mdy")))
    fechas_unicas <- sort(unique(as.Date(fechas[!is.na(fechas)])))
    if (length(fechas_unicas) == 0) return(NULL)
    
    # Permitir selección múltiple
    shinyWidgets::pickerInput(
      inputId = "sesion_cuad",
      label = "Select Session/s:",
      choices = fechas_unicas,
      selected = tail(fechas_unicas, 1),
      multiple = TRUE,  # <---- AHORA PERMITE MULTIPLE
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `style` = "background-color: #1e1e1e; color: #ffffff;"
      )
    )
  })
  
  # Sliders para filtrar valores de cada métrica del cuadrante, alineados en una sola fila con estilo glass
  output$sliders_metricas_cuad <- renderUI({
    req(input$metricas_cuad, length(input$metricas_cuad) == 2, read_data())
    data <- read_data()
    metricas <- input$metricas_cuad
    
    fluidRow(
      lapply(1:2, function(i) {
        metrica <- metricas[i]
        valores <- suppressWarnings(as.numeric(data[[metrica]]))
        if (all(is.na(valores))) return(NULL)
        min_val <- floor(min(valores, na.rm = TRUE))
        max_val <- ceiling(max(valores, na.rm = TRUE))
        column(
          width = 6, style = "padding-left: 8px; padding-right: 8px; margin-bottom: 6px;",
          tags$div(
            style = "background: rgba(14,17,23,0.92); border-radius: 16px; padding: 10px 10px 8px 12px; box-shadow: 0 2px 8px #10101040; min-width:210px;",
            tags$div(
              style = "color:#00FFFF; font-weight:600; font-size:1.1em; margin-bottom:6px;",
              metrica
            ),
            sliderInput(
              inputId = paste0("filtro_valor_", metrica),
              label = tags$span(
                paste0("Filter ", metrica, ":"),
                style = "color:#ffffff; font-size:0.98em;"
              ),
              min = min_val,
              max = max_val,
              value = c(min_val, max_val),
              step = ifelse(max_val - min_val > 10, 1, 0.01),
              width = "100%"
            )
          )
        )
      })
    )
  })
  
  #-------------------------------
  # VALUE BOX METRI OVER TIME 
  #-------------------------------
 
  # 🔷 KPIs glassmorphism arriba del gráfico Time Series
  output$kpi_row_time <- renderUI({
    req(filtro_data(), input$metric, length(input$metric) > 0)
    met_list <- input$metric
    fluidRow(
      style = "margin-bottom: 8px; margin-top: 0px; justify-content:center;",
      lapply(met_list, function(metrica) {
        datos <- filtro_data()[[metrica]]
        n_sessions <- length(unique(filtro_data()[[input$date_col]]))
        mean_val <- round(mean(suppressWarnings(as.numeric(datos)), na.rm = TRUE), 2)
        max_val <- round(max(suppressWarnings(as.numeric(datos)), na.rm = TRUE), 2)
        column(
          width = 4,
          style = "padding: 0 7px;",
          tags$div(
            style = "background: rgba(30,30,30,0.92); border-radius: 18px; box-shadow: 0 2px 8px #10101040; min-width:240px; min-height:110px; padding: 12px 14px 9px 16px; display:flex; flex-direction:column; align-items:center; justify-content:center; margin-bottom:7px;",
            tags$div(
              style = "font-size:1.2em;font-weight:600; margin-bottom:3px;",
              metrica
            ),
            tags$div(
              style = "display:flex; flex-direction:row; gap:16px; justify-content:center; align-items:center;",
              # n sesiones
              tags$div(
                style = "display:flex; flex-direction:column; align-items:center; margin-right:7px;",
                tags$span(icon("calendar-check"), style = "font-size:1.4em; color:#fd002b; margin-bottom:2px;"),
                tags$span(n_sessions, style = "font-size:1.05em; color:#ffffff; font-family:'Inter',sans-serif; font-weight:600;"),
                tags$span("Sessions", style = "font-size:0.92em; color:#c8c8c8;")
              ),
              # Media
              tags$div(
                style = "display:flex; flex-direction:column; align-items:center; margin-right:7px;",
                tags$span(icon("chart-line"), style = "font-size:1.4em; color:#00e676; margin-bottom:2px;"),
                tags$span(mean_val, style = "font-size:1.05em; color:#ffffff; font-family:'Inter',sans-serif; font-weight:600;"),
                tags$span("Mean", style = "font-size:0.92em; color:#c8c8c8;")
              ),
              # Máximo
              tags$div(
                style = "display:flex; flex-direction:column; align-items:center;",
                tags$span(icon("trophy"), style = "font-size:1.4em; color:#7F00FF; margin-bottom:2px;"),
                tags$span(max_val, style = "font-size:1.05em; color:#ffffff; font-family:'Inter',sans-serif; font-weight:600;"),
                tags$span("Max", style = "font-size:0.92em; color:#c8c8c8;")
              )
            )
          )
        )
      })
    )
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
          
          # 💡 FILTRO DE DURACIÓN
          if (!is.null(input$filtro_duracion)) {
            dur <- NULL
            # Si hay columna directa de duración
            if (!is.null(input$duration_col) && input$duration_col != "None" && input$duration_col %in% names(data)) {
              dur <- suppressWarnings(as.numeric(data[[input$duration_col]]))
              # Si hay hora inicio y hora fin
            } else if (!is.null(input$start_col) && input$start_col != "None" &&
                       !is.null(input$end_col) && input$end_col != "None" &&
                       input$start_col %in% names(data) && input$end_col %in% names(data)) {
              hora_inicio <- suppressWarnings(parse_time(data[[input$start_col]]))
              hora_fin <- suppressWarnings(parse_time(data[[input$end_col]]))
              dur <- as.numeric(difftime(hora_fin, hora_inicio, units = "mins"))
            }
            if (!is.null(dur)) {
              keep <- !is.na(dur) & dur >= input$filtro_duracion[1] & dur <= input$filtro_duracion[2]
              data <- data[keep, ]
            }
          }
          
          # Limpieza de datos
          data[[metrica_local]] <- suppressWarnings(as.numeric(data[[metrica_local]]))
          data[[input$date_col]] <- parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
          data <- data[!is.na(data[[metrica_local]]) & !is.na(data[[input$date_col]]), ]
          
          # Aplicar filtro por rango de valores
          val_range <- input[[filtro_id]]
          data <- data[data[[metrica_local]] >= val_range[1] & data[[metrica_local]] <= val_range[2], ]
          
          # Agregación por jugador y fecha
          plot_data <- data %>%
            group_by(Fecha = as.Date(.data[[input$date_col]]), Jugador = .data[[input$player_col]]) %>%
            summarise(Promedio = mean(.data[[metrica_local]], na.rm = TRUE), .groups = "drop") %>%
            mutate(
              Fecha = factor(Fecha, levels = sort(unique(Fecha))),
              tooltip = paste0(
                "<b>Jugador:</b> ", Jugador,
                "<br><b>Fecha:</b> ", Fecha,
                "<br><b>Promedio:</b> ", round(Promedio, 2)
              )
            )
          
          # Gráfico
          p <- ggplot(plot_data, aes(x = Fecha, y = Promedio, fill = Jugador, text = tooltip)) +
            geom_col(position = position_dodge2(preserve = "single"), width = 0.7) +
            scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)]) +
            scale_fill_manual(values = rep("#00FFFF", length(unique(plot_data$Jugador)))) +
            labs(
              title = paste("Promedio de", metrica_local, "por Fecha y Jugador"),
              x = "Fecha", y = metrica_local
            ) +
            theme_minimal(base_size = 14) +
            theme(
              plot.background = element_rect(fill = "transparent", color = NA),
              panel.background = element_rect(fill = "transparent", color = NA),
              panel.grid.major = element_line(color = "#2c2c2c"),
              panel.grid.minor = element_line(color = "#2c2c2c"),
              axis.text.x = element_text(angle = 45, hjust = 1, color = "#ffffff"),
              axis.text.y = element_text(color = "#ffffff"),
              axis.title = element_text(color = "#ffffff", face = "bold"),
              plot.title = element_text(
                hjust = 0.5, face = "bold", size = 20, color = "#00FFFF"
              ),
              legend.position = "none"
            )
          
          ggplotly(p, tooltip = "text") %>%
            layout(
              plot_bgcolor = "transparent",
              paper_bgcolor = "transparent",
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
          
          # Conversión segura y filtrado
          data[[metrica_local]] <- suppressWarnings(as.numeric(data[[metrica_local]]))
          val_range <- input[[filtro_id]]
          data <- data[!is.na(data[[metrica_local]]) & 
                         data[[metrica_local]] >= val_range[1] & 
                         data[[metrica_local]] <= val_range[2], ]
          
          # Preparación del dataset para el gráfico
          plot_data <- data %>%
            mutate(
              MatchDay = as.factor(.data[[input$matchday_col]]),
              Jugador = .data[[input$player_col]],
              Valor = .data[[metrica_local]],
              tooltip = paste0(
                "<b>Jugador:</b> ", Jugador,
                "<br><b>Match Day:</b> ", MatchDay,
                "<br><b>", metrica_local, ":</b> ", round(Valor, 2)
              )
            )
          
          # Gráfico Boxplot
          p <- ggplot(plot_data, aes(x = MatchDay, y = Valor, text = tooltip)) +
            geom_boxplot(
              aes(fill = MatchDay),
              outlier.shape = 21,
              outlier.size = 2,
              outlier.fill = "#ffffff",
              outlier.color = "#fd002b",
              alpha = 0.7
            ) +
            scale_fill_manual(values = rep("#00FFFF", length(unique(plot_data$MatchDay)))) +
            labs(
              title = paste("Distribución de", metrica_local, "por Match Day"),
              x = "Match Day", y = metrica_local
            ) +
            theme_minimal(base_size = 14) +
            theme(
              plot.background = element_rect(fill = "transparent", color = NA),
              panel.background = element_rect(fill = "transparent", color = NA),
              panel.grid.major = element_line(color = "#2c2c2c"),
              panel.grid.minor = element_line(color = "#2c2c2c"),
              axis.text.x = element_text(angle = 45, hjust = 1, color = "#ffffff"),
              axis.text.y = element_text(color = "#ffffff"),
              axis.title = element_text(color = "#ffffff", face = "bold"),
              plot.title = element_text(
                hjust = 0.5, face = "bold", size = 20,
                color = "#00FFFF", family = "Geist"
              ),
              legend.position = "none"
            )
          
          ggplotly(p, tooltip = "text") %>%
            layout(
              plot_bgcolor = "transparent",
              paper_bgcolor = "transparent",
              font = list(color = "#ffffff")
            )
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
          
          # Conversión y filtro por rango de valores
          data[[metrica_local]] <- suppressWarnings(as.numeric(data[[metrica_local]]))
          val_range <- input[[filtro_id]]
          data <- data[!is.na(data[[metrica_local]]) &
                         data[[metrica_local]] >= val_range[1] &
                         data[[metrica_local]] <= val_range[2], ]
          
          # Preparar datos para graficar
          plot_data <- data %>%
            mutate(
              Tarea = as.factor(.data[[input$task_col]]),
              Jugador = .data[[input$player_col]],
              Valor = .data[[metrica_local]],
              tooltip = paste0(
                "<b>Jugador:</b> ", Jugador,
                "<br><b>Tarea:</b> ", Tarea,
                "<br><b>", metrica_local, ":</b> ", round(Valor, 2)
              )
            )
          
          # Crear gráfico
          p <- ggplot(plot_data, aes(x = Tarea, y = Valor, text = tooltip)) +
            geom_boxplot(
              aes(fill = Tarea),
              outlier.shape = 21,
              outlier.size = 2,
              outlier.fill = "#ffffff",
              outlier.color = "#fd002b",
              alpha = 0.7
            ) +
            scale_fill_manual(values = rep("#00FFFF", length(unique(plot_data$Tarea)))) +
            theme_minimal(base_size = 14) +
            labs(
              title = paste("Distribución de", metrica_local, "por Tarea"),
              x = "Tarea", y = metrica_local
            ) +
            theme(
              plot.background = element_rect(fill = "transparent", color = NA),
              panel.background = element_rect(fill = "transparent", color = NA),
              panel.grid.major = element_line(color = "#2c2c2c"),
              panel.grid.minor = element_line(color = "#2c2c2c"),
              axis.text.x = element_text(angle = 45, hjust = 1, color = "#ffffff"),
              axis.text.y = element_text(color = "#ffffff"),
              axis.title = element_text(color = "#ffffff", face = "bold"),
              plot.title = element_text(
                hjust = 0.5, face = "bold", size = 20,
                color = "#00FFFF", family = "Geist"
              ),
              legend.position = "none"
            )
          
          ggplotly(p, tooltip = "text") %>%
            layout(
              plot_bgcolor = "transparent",
              paper_bgcolor = "transparent",
              font = list(color = "#ffffff")
            )
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
          
          data <- filtro_data_z(metrica = metrica_local, rango = input[[filtro_id]])
          if (!(metrica_local %in% names(data))) return(NULL)
          
          data[[input$date_col]] <- parse_date_time(data[[input$date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
          data[[metrica_local]] <- suppressWarnings(as.numeric(data[[metrica_local]]))
          data <- data[!is.na(data[[input$date_col]]) & !is.na(data[[metrica_local]]), ]
          
          val_range <- input[[filtro_id]]
          data <- data[data[[metrica_local]] >= val_range[1] & data[[metrica_local]] <= val_range[2], ]
          
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
              tooltip = paste0(
                "<b>Jugador:</b> ", Jugador,
                "<br><b>Fecha:</b> ", Fecha,
                "<br><b>Z-score:</b> ", round(z, 2)
              )
            ) %>%
            ungroup() %>%
            filter(!is.na(z), is.finite(z))
          
          if (!is.null(input$filtro_jugador_z) && length(input$filtro_jugador_z) > 0) {
            z_data <- z_data %>% filter(Jugador %in% input$filtro_jugador_z)
          } else {
            jugadores_default <- unique(z_data$Jugador)[1:min(12, length(unique(z_data$Jugador)))]
            z_data <- z_data %>% filter(Jugador %in% jugadores_default)
          }
          
          if (nrow(z_data) == 0) return(NULL)
          
          colores_base <- c("Alto" = "#e74c3c", "Bajo" = "#2ecc71", "Neutral" = "#f1c40f")
          fondo_rojo  <- "#fdecea"
          fondo_verde <- "#eafaf1"
          fecha_min <- min(z_data$Fecha, na.rm = TRUE)
          fecha_max <- max(z_data$Fecha, na.rm = TRUE)
          ncol_facetas <- min(4, length(unique(z_data$Jugador)))
          
          p <- ggplot(z_data, aes(x = Fecha, y = z, text = tooltip, color = z_color)) +
            annotate("rect", xmin = fecha_min, xmax = fecha_max,
                     ymin = 1.5, ymax = Inf, fill = fondo_rojo, alpha = 0.4) +
            annotate("rect", xmin = fecha_min, xmax = fecha_max,
                     ymin = -Inf, ymax = -1.5, fill = fondo_verde, alpha = 0.4) +
            geom_smooth(aes(group = Jugador), method = "loess", span = 0.9, se = FALSE,
                        color = "#34495e", linewidth = 0.6) +
            geom_point(size = 1.2) +
            scale_color_manual(values = colores_base, name = "Z-score") +
            geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
            facet_wrap(~Jugador, scales = "free_y", ncol = ncol_facetas) +
            theme_minimal(base_size = 14) +
            labs(
              title = paste("Z-score de", metrica_local, "por jugador"),
              x = "Fecha", y = "Z-score"
            ) +
            theme(
              plot.background = element_rect(fill = "transparent", color = NA),
              panel.background = element_rect(fill = "transparent", color = NA),
              panel.grid.major = element_line(color = "#2c2c2c"),
              panel.grid.minor = element_line(color = "#2c2c2c"),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "#ffffff"),
              axis.text.y = element_text(size = 12, color = "#ffffff"),
              axis.title = element_text(face = "bold", size = 14, color = "#ffffff"),
              plot.title = element_text(hjust = 0.5, face = "bold", size = 20, color = "#00FFFF", family = "Geist"),
              strip.text = element_text(face = "bold", size = 13, color = "#ffffff"),
              legend.position = "bottom",
              legend.text = element_text(color = "#ffffff"),
              legend.title = element_text(color = "#ffffff", face = "bold")
            )
          
          ggplotly(p, tooltip = "text") %>%
            layout(
              plot_bgcolor = "transparent",
              paper_bgcolor = "transparent",
              font = list(color = "#ffffff"),
              legend = list(orientation = "h", x = 0.3, y = -0.2)
            )
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
          
          p <- ggplot(resumen, aes(
            x = Jugador,
            y = Valor,
            fill = Valor,
            text = paste0("<b>Jugador:</b> ", Jugador, "<br><b>Valor:</b> ", round(Valor, 2))
          )) +
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
              x = "Jugador", y = metrica_local
            ) +
            theme(
              plot.background = element_rect(fill = "transparent", color = NA),
              panel.background = element_rect(fill = "transparent", color = NA),
              panel.grid.major = element_line(color = "#2c2c2c"),
              panel.grid.minor = element_line(color = "#2c2c2c"),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "#ffffff"),
              axis.text.y = element_text(size = 12, color = "#ffffff"),
              axis.title = element_text(face = "bold", size = 14, color = "#ffffff"),
              plot.title = element_text(
                hjust = 0.5, face = "bold", size = 20,
                color = "#00FFFF", family = "Inter"
              )
            )
          
          ggplotly(p, tooltip = "text") %>%
            layout(
              plot_bgcolor = "transparent",
              paper_bgcolor = "transparent",
              font = list(color = "#ffffff")
            )
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
          
          # Parsear fechas
          data_full[[date_col]] <- parse_date_time(data_full[[date_col]], orders = c("Y-m-d", "d-m-Y", "m/d/Y"))
          data_full <- data_full[!is.na(data_full[[date_col]]), ]
          
          # Filtrar por Match Day si corresponde
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
          
          # Calcular stats rolling (antes del partido)
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
          
          # Consolidar valor de sesión
          data_sesion <- data_full %>%
            filter(as.character(.data[[date_col]]) == input$filtro_sesion_selector_comp) %>%
            group_by(Jugador = .data[[player_col]]) %>%
            summarise(Valor = mean(.data[[metrica_local]], na.rm = TRUE), .groups = "drop")
          
          # Calcular Z-score final
          data_final <- left_join(data_sesion, stats_movil, by = "Jugador") %>%
            mutate(
              z = (Valor - media_movil) / sd_movil,
              z_color = case_when(
                z >= 1.5 ~ "Alto",
                z <= -1.5 ~ "Bajo",
                TRUE ~ "Neutral"
              ),
              tooltip = paste0("Jugador: ", Jugador, "<br>Z-score: ", round(z, 2))
            ) %>%
            filter(!is.na(z) & is.finite(z)) %>%
            arrange(z) %>%
            mutate(Jugador = factor(Jugador, levels = unique(Jugador)))
          
          if (nrow(data_final) == 0) {
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
              plot.background = element_rect(fill = "transparent", color = NA),
              panel.background = element_rect(fill = "transparent", color = NA),
              panel.grid.major = element_line(color = "#2c2c2c"),
              panel.grid.minor = element_line(color = "#2c2c2c"),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "#ffffff"),
              axis.text.y = element_text(size = 12, color = "#ffffff"),
              axis.title = element_text(face = "bold", size = 14, color = "#ffffff"),
              plot.title = element_text(
                hjust = 0.5, face = "bold", size = 20, color = "#00FFFF",
                family = "Inter"
              ),
              legend.position = "right",
              legend.text = element_text(color = "#ffffff"),
              legend.title = element_text(color = "#ffffff", face = "bold")
            )
          
          ggplotly(p, tooltip = "text") %>%
            layout(
              plot_bgcolor = "rgba(0,0,0,0)",
              paper_bgcolor = "rgba(0,0,0,0)",
              font = list(color = "#ffffff"),
              margin = list(l = 40, r = 40, b = 60, t = 80)
            )
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
    
    # Tabla de datos con estilo LIFT
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
      # 🎨 Estilo general: fondo oscuro, texto blanco, fuente y tamaño
      formatStyle(
        columns = names(resumen_final),
        backgroundColor = '#1e1e1e',
        color = '#ffffff',
        fontFamily = 'Open Sans',
        fontSize = '14px'
      ) %>%
      # 🎯 Estilo de Z-score (condicional por tramos)
      formatStyle(
        'Z_score',
        backgroundColor = styleInterval(
          c(-1.5, 1.5),
          c('#2ecc71', '#f1c40f', '#e74c3c')
        ),
        color = '#000000',
        fontWeight = 'bold'
      )
  })
  
  # 🔵 Output: Gráfico de ACWR Exponencial
  observe({
    req(input$metric_acwr, input$acwr_agudo_dias, input$acwr_cronico_dias)
    
    # Funcion EWMA personalizada
    ewma_custom <- function(x, lambda) {
      out <- numeric(length(x))
      out[1] <- x[1]
      for (i in 2:length(x)) {
        out[i] <- lambda * x[i] + (1 - lambda) * out[i - 1]
      }
      return(out)
    }
    
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
              EWMA_agudo = ewma_custom(.data[[metrica_local]], lambda_agudo),
              EWMA_cronico = ewma_custom(.data[[metrica_local]], lambda_cronico),
              ACWR = EWMA_agudo / EWMA_cronico
            ) %>%
            ungroup() %>%
            filter(!is.na(ACWR), is.finite(ACWR)) %>%
            mutate(
              Fecha = as.Date(.data[[input$date_col]]),
              tooltip = paste0("Jugador: ", Jugador, "<br>Fecha: ", Fecha, "<br>ACWR: ", round(ACWR, 2)),
              color = case_when(
                ACWR > 1.5 ~ "#fd002b",
                ACWR >= 0.8 & ACWR <= 1.5 ~ "#eafaf1",
                TRUE ~ "#fd002b"
              )
            )
          
          if (nrow(acwr_data) == 0) return(NULL)
          
          # Asignar colores según rangos
          acwr_data <- acwr_data %>%
            mutate(color = case_when(
              ACWR < 0.8 ~ "#ffcccc",           # Rojo suave
              ACWR >= 0.8 & ACWR <= 1.5 ~ "#ccffcc",  # Verde suave
              ACWR > 1.5 ~ "#fd002b"             # Rojo fuerte
            ))
          
          # Gráfico
          p <- ggplot(acwr_data, aes(x = Fecha, y = ACWR, text = tooltip)) +
            
            # Puntos coloreados según valor de ACWR
            geom_point(aes(color = color), size = 2.2, show.legend = FALSE) +
            
            # Línea suavizada de tendencia por jugador (blanca)
            geom_smooth(
              aes(group = Jugador), method = "loess", span = 0.8, se = TRUE,
              color = "#ffffff", size = 0.8
            ) +
            
            # Facetas por jugador
            facet_wrap(~Jugador, ncol = 4, scales = "fixed") +
            
            # Colores directamente desde la columna
            scale_color_identity() +
            
            # Estilo general
            expand_limits(y = 0) +
            theme_minimal(base_size = 14) +
            labs(
              title = paste("ACWR Exponencial –", metrica_local),
              x = "Fecha", y = "ACWR"
            ) +
            theme(
              plot.background = element_rect(fill = "transparent", color = NA),
              panel.background = element_rect(fill = "transparent", color = NA),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(color = "#2c2c2c"),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "#ffffff"),
              axis.text.y = element_text(size = 11, color = "#ffffff"),
              axis.title = element_text(face = "bold", size = 14, color = "#ffffff"),
              plot.title = element_text(
                hjust = 0.5, face = "bold", size = 20, color = "#00FFFF",
                family = "Inter"
              ),
              strip.text = element_text(size = 12, face = "bold", color = "#ffffff"),
              strip.background = element_blank(),
              legend.position = "none"
            )
          
          # Convertir a gráfico interactivo
          ggplotly(p, tooltip = "text") %>%
            layout(
              plot_bgcolor = "transparent",
              paper_bgcolor = "transparent",
              font = list(color = "#ffffff")
            )
        })
        
        #' 🔴 Output: Gráfico de ratio Partido vs Semana
        #'
        #' Muestra un gráfico de barras por jugador con el ratio:
        #' - acumulado de días seleccionados / promedio rolling de partidos (MD)
        #' - colores: rojo > 1.2, verde < 0.8, gris intermedio
        #' - usa filtros de jugador, puesto, tarea, duración, fechas seleccionadas y ventana MD
        
        observe({
          req(input$metricas)  # ← Solo necesita el mapeo del lateral
          updateSelectInput(
            session,
            inputId = "metricas_microciclo",
            choices = input$metricas,
            selected = input$metricas[1]
          )
        })
        
        # 📦 UI para el gráfico
        output$microciclo_ratio_plot_ui <- renderUI({
          req(input$metricas_microciclo)
          plotlyOutput("plot_microciclo_ratio", height = "600px")
        })
        
        # 📊 Render del gráfico
        output$plot_microciclo_ratio <- renderPlotly({
          req(read_data(), input$metricas_microciclo, input$ventana_movil_micro)
          
          data <- read_data()
          
          # 📆 Parsear fechas y guardar como nueva columna
          data$fecha_parsed <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("ymd", "dmy", "mdy")))
          data <- data[!is.na(data$fecha_parsed), ]
          
          # 🎯 Filtrar por duración (si existe)
          if (!is.null(input$duration_col) && input$duration_col != "None") {
            data <- data %>%
              filter(
                !is.na(.data[[input$duration_col]]),
                .data[[input$duration_col]] >= input$filtro_duracion_micro[1],
                .data[[input$duration_col]] <= input$filtro_duracion_micro[2]
              )
          } else if (!is.null(input$start_col) && !is.null(input$end_col)) {
            hora_inicio <- parse_time(data[[input$start_col]])
            hora_fin <- parse_time(data[[input$end_col]])
            duracion <- as.numeric(difftime(hora_fin, hora_inicio, units = "mins"))
            data <- data[!is.na(duracion) & duracion >= input$filtro_duracion_micro[1] & duracion <= input$filtro_duracion_micro[2], ]
          }
          
          # 👤 Filtros adicionales
          if (!is.null(input$filtro_jugador_micro)) {
            data <- data[data[[input$player_col]] %in% input$filtro_jugador_micro, ]
          }
          if (!is.null(input$filtro_tarea_micro)) {
            data <- data[data[[input$task_col]] %in% input$filtro_tarea_micro, ]
          }
          if (!is.null(input$filtro_puesto_micro)) {
            data <- data[data[[input$position_col]] %in% input$filtro_puesto_micro, ]
          }
          
          # ⚽ Clasificar tipo de sesión: múltiples variantes posibles de partido
          valores_partido <- c("md", "match", "game", "partido", "juego")
          
          data$tipo <- ifelse(
            tolower(trimws(as.character(data[[input$matchday_col]]))) %in% valores_partido,
            "partido",
            "entreno"
          )
          
          # 📈 Calcular ratios para cada métrica
          resultados <- lapply(input$metricas_microciclo, function(metrica) {
            if (!metrica %in% names(data)) return(NULL)
            
            # Leer umbrales para esta métrica
            umbral_bajo <- input[[paste0("umbral_bajo_", metrica)]]
            umbral_alto <- input[[paste0("umbral_alto_", metrica)]]
            if (is.null(umbral_bajo)) umbral_bajo <- 0.8
            if (is.null(umbral_alto)) umbral_alto <- 1.5
            
            # 📌 Subsets de datos
            partidos_raw <- data %>% filter(tipo == "partido")
            entrenos <- data %>% filter(tipo == "entreno", as.Date(fecha_parsed) %in% as.Date(input$fechas_entreno_micro))
            
            # Promedio de partidos
            partidos_filtrados <- partidos_raw %>%
              arrange(.data[[input$player_col]], desc(fecha_parsed)) %>%
              group_by(Jugador = .data[[input$player_col]]) %>%
              mutate(rn = row_number()) %>%
              filter(rn <= input$ventana_movil_micro) %>%
              ungroup()
            
            partidos_resumen <- partidos_filtrados %>%
              group_by(Jugador) %>%
              summarise(partido = mean(.data[[metrica]], na.rm = TRUE), .groups = "drop")
            
            # Acumulado de entrenamientos
            entreno_resumen <- entrenos %>%
              group_by(Jugador = .data[[input$player_col]]) %>%
              summarise(entreno = sum(.data[[metrica]], na.rm = TRUE), .groups = "drop")
            
            # Verificación de jugadores con pocos partidos
            conteo_partidos <- partidos_filtrados %>%
              group_by(Jugador) %>%
              summarise(n = n(), .groups = "drop") %>%
              filter(n < input$ventana_movil_micro)
            
            if (nrow(conteo_partidos) > 0) {
              showNotification(
                paste0("⚠️ Jugadores con menos de ", input$ventana_movil_micro, " partidos: ",
                       paste(conteo_partidos$Jugador, collapse = ", ")),
                type = "warning",
                duration = 3
              )
            }
            # Calcular ratio con umbrales personalizados
            df <- inner_join(partidos_resumen, entreno_resumen, by = "Jugador") %>%
              filter(!is.na(partido), !is.na(entreno), entreno > 0) %>%
              mutate(
                metrica = metrica,
                ratio = entreno / partido,
                color_label = case_when(
                  ratio > umbral_alto ~ "Alto",
                  ratio < umbral_bajo ~ "Bajo",
                  TRUE ~ "Normal"
                ),
                # 🔴 Tooltip detallado
                tooltip = paste0(
                  "Jugador: ", Jugador,
                  "<br>Acumulado: ", round(entreno, 1),
                  "<br>Partido: ", round(partido, 1),
                  "<br>Ratio: ", round(ratio, 2)
                )
              )
            return(df)
          })
          
          df <- bind_rows(resultados)
          if (nrow(df) == 0) {
            showNotification("❌ No hay datos para graficar. Ajustá los filtros o seleccioná otras fechas o métricas.", type = "error", duration = 8)
            return(NULL)
          }
          
          # Colores SIEMPRE los 3 labels, en el orden correcto
          scale_colors <- c("Alto" = "#fd002b", "Bajo" = "#00e676", "Normal" = "#c8c8c8")
          
          # 📊 GRAFICO
          p <- ggplot(df, aes(
            x = Jugador,
            y = ratio,
            fill = color_label,
            text = tooltip           # 👈 Tooltip detallado
          )) +
            geom_col(width = 0.8) +
            facet_wrap(~metrica, scales = "free_y") +
            geom_hline(yintercept = 1, linetype = "dashed", color = "#ffffff") +
            scale_fill_manual(values = scale_colors, name = "Ratio") +
            labs(title = "⚖️ Ratio Partido vs Semana", x = "Jugador", y = "Ratio (Partido / Entreno)") +
            theme_minimal(base_size = 14) +
            theme(
              plot.background = element_rect(fill = "transparent", color = NA),
              panel.background = element_rect(fill = "transparent", color = NA),
              panel.grid.major = element_line(color = "#2c2c2c"),
              panel.grid.minor = element_line(color = "#2c2c2c"),
              axis.text.x = element_text(angle = 45, hjust = 1, color = "#ffffff"),
              axis.text = element_text(color = "#ffffff"),
              axis.title = element_text(color = "#ffffff", face = "bold"),
              strip.text = element_text(color = "#ffffff", face = "bold"),
              plot.title = element_text(color = "#00FFFF", face = "bold", hjust = 0.5),
              legend.title = element_text(color = "#ffffff", family = "Space Grotesk", size = 16),
              legend.text = element_text(color = "#ffffff", family = "Inter", size = 14)
            )
          
          ggplotly(p, tooltip = "text") %>%
            layout(
              plot_bgcolor = "rgba(0,0,0,0)",
              paper_bgcolor = "rgba(0,0,0,0)",
              font = list(
                color = "#ffffff",
                size = 15
              ),
              showlegend = FALSE  # <--- Oculta la leyenda, como tenías
            )
        })
      })
    }
  })
  
  #' 🟦 Output: Gráfico de Cuadrante
  #'
  #' Visualiza a cada jugador en un plano de dos métricas seleccionadas para una sesión específica.
  #'
  #' - Cada punto representa un jugador y su valor para Métrica X (eje X) y Métrica Y (eje Y).
  #' - El cuadrante está delimitado por la **mediana** de cada métrica:
  #'   - "Alto-Alto": ambos valores por encima de la mediana.
  #'   - "Bajo-Bajo": ambos por debajo de la mediana.
  #'   - "Alto-Bajo" y "Bajo-Alto": uno por arriba y otro por debajo.
  #' - Los cuadrantes se identifican con colores (paleta LIFT): rojo, verde, cyan, violeta.
  #' - Permite filtrar por jugador, puesto, tarea, duración y sesión.
  #' - Sólo permite elegir **2 métricas** a la vez.
  #' - Tooltip con: nombre, valor X, valor Y, cuadrante.
  #' - El gráfico se renderiza con ggplot2 + plotly, respetando modo oscuro y estilo personalizado.
  # UI: Output dinámico para el gráfico (ubicado en la columna derecha del tab)
  
  #-------------------------------------------
  # OBSERVER PARA SELECTOR DE MÉTRICAS CUADRANTE
  #-------------------------------------------
  
  observe({
    req(input$metric_col)
    updateSelectInput(
      session,
      inputId = "metricas_cuad",
      choices = input$metric_col,
      selected = input$metric_col[1:2]
    )
  })
  
  # UI Output para el plot
  output$cuadrante_plot_ui <- renderUI({
    req(input$metricas_cuad)
    if (length(input$metricas_cuad) != 2) {
      return(tags$p("Seleccioná exactamente 2 métricas para visualizar el cuadrante.", style = "color:#fd002b; font-size:1.1em; font-weight:600; text-align:center; margin-top:2em;"))
    }
    plotlyOutput("plot_cuadrante", height = "600px")
  })
  
  # Render plotly
  output$plot_cuadrante <- renderPlotly({
    req(read_data(), input$metricas_cuad)
    if (length(input$metricas_cuad) != 2) return(NULL)
    
    data <- read_data()
    data$fecha_parsed <- suppressWarnings(parse_date_time(data[[input$date_col]], orders = c("ymd", "dmy", "mdy")))
    
    
    # Filtros por sesión
    if (!is.null(input$sesion_cuad) && length(input$sesion_cuad) > 0) {
      data <- data[as.Date(data$fecha_parsed) %in% as.Date(input$sesion_cuad), ]
    }
    # Filtros adicionales
    if (!is.null(input$filtro_jugador_cuad)) {
      data <- data[data[[input$player_col]] %in% input$filtro_jugador_cuad, ]
    }
    if (!is.null(input$filtro_puesto_cuad)) {
      data <- data[data[[input$position_col]] %in% input$filtro_puesto_cuad, ]
    }
    if (!is.null(input$filtro_tarea_cuad)) {
      data <- data[data[[input$task_col]] %in% input$filtro_tarea_cuad, ]
    }
    if (!is.null(input$filtro_duracion_cuad)) {
      dur <- NULL
      if (!is.null(input$duration_col) && input$duration_col != "None") {
        dur <- suppressWarnings(as.numeric(data[[input$duration_col]]))
      } else if (!is.null(input$start_col) && !is.null(input$end_col)) {
        hora_inicio <- suppressWarnings(parse_time(data[[input$start_col]]))
        hora_fin <- suppressWarnings(parse_time(data[[input$end_col]]))
        dur <- as.numeric(difftime(hora_fin, hora_inicio, units = "mins"))
      }
      if (!is.null(dur)) {
        data <- data[!is.na(dur) & dur >= input$filtro_duracion_cuad[1] & dur <= input$filtro_duracion_cuad[2], ]
      }
    }
    
    # Validar columnas
    met_x <- input$metricas_cuad[1]
    met_y <- input$metricas_cuad[2]
    req(met_x %in% names(data), met_y %in% names(data))
    
    for (metrica in input$metricas_cuad) {
      slider_id <- paste0("filtro_valor_", metrica)
      if (!is.null(input[[slider_id]])) {
        rango <- input[[slider_id]]
        data <- data[data[[metrica]] >= rango[1] & data[[metrica]] <= rango[2], ]
      }
    }
    
    # 2. Calcular mediana de cada métrica
    x_med <- median(data[[met_x]], na.rm = TRUE)
    y_med <- median(data[[met_y]], na.rm = TRUE)
    
    # 3. Crear variable de cuadrante para colores y labels
    data$cuadrante <- dplyr::case_when(
      data[[met_x]] >= x_med & data[[met_y]] >= y_med ~ "Alto-Alto",
      data[[met_x]] <  x_med & data[[met_y]] >= y_med ~ "Bajo-Alto",
      data[[met_x]] <  x_med & data[[met_y]] <  y_med ~ "Bajo-Bajo",
      data[[met_x]] >= x_med & data[[met_y]] <  y_med ~ "Alto-Bajo"
    )
    data$cuadrante <- factor(data$cuadrante, levels = c("Alto-Alto", "Bajo-Alto", "Bajo-Bajo", "Alto-Bajo"))
    
    # 4. Paleta de colores cuadrantes (LIFT)
    colores_cuadrante <- c(
      "Alto-Alto" = "#fd002b",  # Rojo
      "Bajo-Alto" = "#7F00FF",  # Violeta
      "Bajo-Bajo" = "#00e676",  # Verde
      "Alto-Bajo" = "#00FFFF"   # Cyan
    )
    
    # 5. Armar plot
    p <- ggplot(data, aes(
      x = .data[[met_x]],
      y = .data[[met_y]],
      color = cuadrante,
      label = .data[[input$player_col]],
      text = paste0(
        "Jugador: ", .data[[input$player_col]], "<br>",
        met_x, ": ", round(.data[[met_x]], 2), "<br>",
        met_y, ": ", round(.data[[met_y]], 2), "<br>",
        "Cuadrante: ", cuadrante
      )
    )) +
      geom_point(size = 5, alpha = 0.95) +
      scale_color_manual(values = colores_cuadrante, name = "Cuadrante") +
      geom_vline(xintercept = x_med, linetype = "dashed", color = "#c8c8c8") +
      geom_hline(yintercept = y_med, linetype = "dashed", color = "#c8c8c8") +
      labs(
        title = paste0("Gráfico de Cuadrante: ", met_x, " vs ", met_y),
        x = met_x, y = met_y
      ) +
      theme_minimal(base_size = 15) +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_line(color = "#1e1e1e"),
        panel.grid.minor = element_line(color = "#2c2c2c"),
        axis.text = element_text(color = "#ffffff"),
        axis.title = element_text(color = "#ffffff", face = "bold"),
        plot.title = element_text(color = "#00FFFF", face = "bold", hjust = 0.5),
        legend.title = element_text(color = "#ffffff",  size = 15),
        legend.text = element_text(color = "#ffffff",  size = 13)
      )
    
    # 6. Render con plotly
    ggplotly(p, tooltip = "text") %>%
      layout(
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        font = list(
          color = "#ffffff",
          size = 14
        ),
        legend = list(
          font = list(color = "#ffffff"),
          bgcolor = "rgba(0,0,0,0)"
        )
      )
  })
}
 

shinyApp(ui, server)


