lift_filter_rail <- function(kicker, title, description, ids = character(0), metric_id = NULL,
                             metric_label = "Métricas", extra = NULL) {
  controls <- list(
    tags$div(
      class = "lift-rail-heading",
      tags$span(class = "lift-eyebrow", kicker),
      tags$h2(title),
      tags$p(description)
    )
  )
  controls <- c(
    controls,
    lapply(ids, function(id) {
      tags$div(class = "lift-field", uiOutput(paste0("filtro_", id)))
    })
  )
  if (!is.null(metric_id)) {
    controls <- c(
      controls,
      list(tags$div(
        class = "lift-field lift-metric-field",
        selectInput(metric_id, metric_label, choices = NULL, multiple = TRUE)
      ))
    )
  }
  if (!is.null(extra)) controls <- c(controls, list(extra))
  do.call(tagList, controls)
}

lift_panel_header <- function(kicker, title, description = NULL) {
  tagList(
    tags$div(
      class = "lift-panel-heading",
      tags$div(class = "lift-heading-line", tags$span(class = "lift-eyebrow", kicker)),
      tags$h2(title),
      if (!is.null(description)) tags$p(description)
    )
  )
}

lift_analysis_tab <- function(tab_title, icon_name, kicker, title, description, filter_ui,
                              kpi_id = NULL, plot_ui_id = NULL) {
  tabPanel(
    title = tagList(tags$i(class = paste("bi", icon_name), `aria-hidden` = "true"),
                    tags$span(class = "lift-tab-label", tab_title)),
    tags$div(
      class = "lift-tab-intro",
      tags$span(class = "lift-eyebrow", kicker),
      tags$h1(title),
      tags$p(description)
    ),
    tags$div(
      class = "lift-analysis-grid",
      tags$aside(class = "lift-filter-panel", filter_ui),
      tags$section(
        class = "lift-output-panel",
        if (!is.null(kpi_id)) tags$div(class = "lift-kpi-strip", uiOutput(kpi_id)),
        if (!is.null(plot_ui_id)) tags$div(class = "lift-chart-stack", uiOutput(plot_ui_id))
      )
    )
  )
}

lift_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = NULL,
  bg = "#0B0F12",
  fg = "#F2F6F3",
  primary = "#C9FF35",
  secondary = "#87E8E3",
  base_font = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Space Grotesk")
)

ui <- fluidPage(
  theme = lift_theme,
  tags$head(
    tags$meta(name = "description", content = "LIFT GPS performance workspace for importing, mapping and interpreting athlete-load data."),
    tags$link(rel = "icon", type = "image/png", href = "logo.png"),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"),
    tags$link(rel = "stylesheet", href = "lift-theme.css"),
    tags$script(HTML("document.documentElement.lang = 'es';"))
  ),

  tags$a(class = "lift-skip-link", href = "#lift-workspace", "Saltar al contenido"),

  tags$header(
    class = "lift-topbar",
    tags$a(
      class = "lift-brand",
      href = "#lift-workspace",
      tags$img(src = "logo.png", alt = "LIFT", class = "lift-brand-mark"),
      tags$span(class = "lift-brand-copy", tags$strong("LIFT"), tags$small("GPS performance workspace"))
    ),
    tags$div(
      class = "lift-topbar-meta",
      tags$span(class = "lift-system-label", "DATA / LOAD / DECISION"),
      tags$div(class = "lift-status-chip", tags$span(class = "lift-status-dot"), uiOutput("estado_base"))
    )
  ),

  tags$main(
    id = "lift-workspace",
    class = "lift-shell",
    tags$section(
      class = "lift-hero",
      tags$div(
        class = "lift-hero-copy",
        tags$span(class = "lift-eyebrow", "LIFT performance systems / 01"),
        tags$h1("From GPS export", tags$br(), tags$em("to decision.")),
        tags$p("Importá la carga de Catapult, WIMU o Stats Sports. Mapeá el archivo una vez y trabajá con el mismo lenguaje en cada dashboard."),
        tags$div(
          class = "lift-hero-actions",
          tags$a(class = "lift-text-link", href = "#lift-source", tags$span("Empezar por los datos"), tags$i(class = "bi bi-arrow-down-right", `aria-hidden` = "true")),
          tags$span(class = "lift-hero-note", "sesión local · datos en memoria")
        )
      ),
      tags$div(
        class = "lift-hero-readout",
        tags$div(class = "lift-readout-index", "GPS / 00"),
        tags$div(class = "lift-readout-line"),
        tags$div(class = "lift-readout-copy", "A single view for raw exports, session context and player-level decisions."),
        tags$div(class = "lift-readout-mark", tags$span("L"), tags$span("/"), tags$span("GPS"))
      )
    ),

    tags$section(
      id = "lift-source",
      class = "lift-source-zone",
      tags$div(
        class = "lift-section-heading",
        tags$div(
          tags$span(class = "lift-eyebrow", "01 / Ingesta"),
          tags$h2("Traé los datos al workspace"),
          tags$p("Podés cargar archivos locales o leer una hoja de Google. La base se acumula durante esta sesión.")
        ),
        tags$span(class = "lift-section-index", "SOURCE")
      ),
      tags$div(
        class = "lift-source-grid",
        tags$article(
          class = "lift-source-card lift-source-card-primary",
          tags$div(class = "lift-card-kicker", tags$i(class = "bi bi-cloud-arrow-down", `aria-hidden` = "true"), "Fuente conectada"),
          textInput("google_sheet_url", "Google Sheet URL o ID", value = "", placeholder = "Pegá una URL o un ID de spreadsheet"),
          actionButton("load_google_sheet", "Cargar Google Sheet", icon = icon("arrow-right"), class = "lift-button lift-button-primary"),
          tags$small(class = "lift-helper-copy", "Solo se aceptan fuentes de docs.google.com/spreadsheets.")
        ),
        tags$article(
          class = "lift-source-card lift-file-card",
          tags$div(class = "lift-card-kicker", tags$i(class = "bi bi-file-earmark-bar-graph", `aria-hidden` = "true"), "Archivos locales"),
          fileInput("file", "CSV, XLSX o JSON", multiple = TRUE, accept = c(".csv", ".xlsx", ".json"), buttonLabel = "Elegir archivos", placeholder = "Arrastrá o seleccioná uno o varios archivos"),
          tags$small(class = "lift-helper-copy", "Se conserva el origen y solo se eliminan filas completamente idénticas.")
        ),
        tags$article(
          class = "lift-source-card lift-source-card-status",
          tags$div(class = "lift-card-kicker", tags$i(class = "bi bi-sliders2-vertical", `aria-hidden` = "true"), "Preparación"),
          uiOutput("file_info"),
          actionButton("reset_base", "Vaciar workspace", icon = icon("trash"), class = "lift-button lift-button-quiet"),
          tags$small(class = "lift-helper-copy", "El reset limpia la base en memoria y los mapeos actuales.")
        )
      ),
      tags$details(
        class = "lift-mapping-drawer",
        tags$summary(tags$span(class = "lift-drawer-icon", tags$i(class = "bi bi-diagram-3", `aria-hidden` = "true")),
                     tags$span(tags$strong("Mapear columnas"), tags$small(" Confirmá jugador, fecha, sesión y métricas")),
                     tags$i(class = "bi bi-chevron-down lift-drawer-chevron", `aria-hidden` = "true")),
        tags$div(class = "lift-mapping-content", uiOutput("column_mapping"))
      )
    ),

    uiOutput("data_empty_state"),

    tags$section(
      class = "lift-dashboard-zone",
      tags$div(
        class = "lift-section-heading lift-dashboard-heading",
        tags$div(
          tags$span(class = "lift-eyebrow", "02 / Lectura"),
          tags$h2("Elegí el nivel de lectura"),
          tags$p("Del registro crudo a la carga contextualizada. Los filtros viven junto al gráfico para mantener la decisión visible.")
        ),
        tags$span(class = "lift-section-index", "ANALYSIS")
      ),
      tabsetPanel(
        type = "tabs",
        id = "main_tabs",
        tabPanel(
          title = tagList(tags$i(class = "bi bi-table", `aria-hidden` = "true"), tags$span(class = "lift-tab-label", "Datos")),
          tags$div(class = "lift-tab-intro", tags$span(class = "lift-eyebrow", "Registro"), tags$h1("Datos importados"), tags$p("Revisá columnas, tipos y procedencia antes de entrar en el análisis.")),
          tags$div(class = "lift-data-view", tags$div(class = "lift-table-frame", DT::DTOutput("table")))
        ),
        lift_analysis_tab("Tendencias", "bi-graph-up", "Carga", "Tendencias por fecha", "Compará la evolución diaria y detectá cambios de carga sin salir del contexto del jugador.",
                          lift_filter_rail("Filtros", "Acotar la serie", "Seleccioná la población antes de leer la curva.", c("jugador", "puesto", "matchday", "tarea", "fecha", "duracion"), "metric", "Métricas"),
                          "kpi_row_time", "barras_fecha_ui"),
        lift_analysis_tab("Match day", "bi-box", "Distribución", "Carga por match day", "Entendé la dispersión de cada métrica en relación con el día de partido.",
                          lift_filter_rail("Filtros", "Comparar ventanas", "La distribución necesita una misma unidad de comparación.", c("jugador_box", "puesto_box", "matchday_box", "tarea_box", "fecha_box", "duracion_box"), "metric_box", "Métricas"),
                          "kpi_row_boxplot_md", "boxplot_matchday_ui"),
        lift_analysis_tab("Tareas", "bi-box-seam", "Contexto", "Distribución por tarea", "Separá la carga de campo, gimnasio y tareas específicas para evitar mezclar estímulos.",
                          lift_filter_rail("Filtros", "Aislar el estímulo", "La tarea y el match day definen el contexto de la carga.", c("jugador_task", "puesto_task", "matchday_task", "tarea_task", "fecha_task", "duracion_task"), "metric_task", "Métricas"),
                          "kpi_row_boxplot_task", "boxplot_task_ui"),
        lift_analysis_tab("Z-score", "bi-activity", "Referencia", "Desvío respecto de la historia", "La ventana móvil compara cada observación con las sesiones anteriores del mismo jugador.",
                          lift_filter_rail("Filtros", "Construir la referencia", "Usá la ventana para decidir cuánta historia entra en la comparación.", c("jugador_z", "puesto_z", "matchday_z", "tarea_z", "fecha_z", "duracion_z"), "metric_z", "Métricas",
                                           sliderInput("ventana_movil_z", "Ventana móvil (sesiones)", min = 3, max = 10, value = 5, step = 1)),
                          "kpi_row_zscore_time", "zscore_plot_ui"),
        lift_analysis_tab("Sesión", "bi-calendar2-event", "Detalle", "Una sesión, en contexto", "Leé el rendimiento de una fecha concreta y contrastalo con la referencia disponible.",
                          lift_filter_rail("Filtros", "Elegir la sesión", "Primero seleccioná fecha y población; después, la métrica.", c("jugador_sesion", "puesto_sesion", "matchday_sesion", "tarea_sesion", "duracion_sesion"), "metricas_sesion_plot", "Métricas",
                                           tagList(tags$div(class = "lift-field", uiOutput("filtro_sesion_selector")), tags$div(class = "lift-field", uiOutput("filtro_fecha_sesion")))),
                          "kpi_row_sesion", "graficos_metricas_sesion"),
        lift_analysis_tab("Competencia", "bi-trophy", "Benchmark", "El partido contra tu propia historia", "Calculá el desvío del partido seleccionado contra los match days previos del jugador.",
                          lift_filter_rail("Filtros", "Definir el benchmark", "El partido elegido es el punto de comparación.", character(0), "metric_z_comp", "Métricas",
                                           tagList(tags$div(class = "lift-field", uiOutput("filtro_jugador_z_comp")), tags$div(class = "lift-field", uiOutput("filtro_puesto_z_comp")), tags$div(class = "lift-field", uiOutput("filtro_tarea_z_comp")), tags$div(class = "lift-field", uiOutput("filtro_sesion_selector_comp")), tags$div(class = "lift-field", uiOutput("filtro_duracion_z_comp")), tags$div(class = "lift-field", sliderInput("ventana_movil_z_comp", "Ventana MD móvil", min = 3, max = 5, value = 3, step = 1)))),
                          "kpi_row_competitive", "zscore_comp_plot_ui"),
        tabPanel(
          title = tagList(tags$i(class = "bi bi-lightning-charge", `aria-hidden` = "true"), tags$span(class = "lift-tab-label", "ACWR")),
          tags$div(class = "lift-tab-intro", tags$span(class = "lift-eyebrow", "Carga acumulada"), tags$h1("Acute / chronic workload"), tags$p("Ajustá las ventanas y observá la relación entre carga aguda y crónica por jugador.")),
          tags$div(class = "lift-analysis-grid", tags$aside(class = "lift-filter-panel", lift_filter_rail("Filtros", "Elegir la población", "La lectura empieza por el grupo que querés comparar.", c("jugador_acwr", "puesto_acwr", "matchday_acwr", "tarea_acwr", "fecha_acwr", "duracion_acwr"), "metric_acwr", "Métricas")), tags$section(class = "lift-output-panel", tags$div(class = "lift-control-dock", tags$div(class = "lift-control-dock-item", tags$span(class = "lift-control-label", "Acute"), sliderInput("acwr_agudo_dias", "Días", min = 3, max = 14, value = 7, step = 1)), tags$div(class = "lift-control-dock-item", tags$span(class = "lift-control-label", "Chronic"), sliderInput("acwr_cronico_dias", "Días", min = 14, max = 42, value = 28, step = 1))), tags$div(class = "lift-kpi-strip", uiOutput("kpi_row_acwr")), tags$div(class = "lift-chart-stack", uiOutput("acwr_plot_ui"))))
        ),
        tabPanel(
          title = tagList(tags$i(class = "bi bi-bar-chart-line", `aria-hidden` = "true"), tags$span(class = "lift-tab-label", "Microciclo")),
          tags$div(class = "lift-tab-intro", tags$span(class = "lift-eyebrow", "Semana"), tags$h1("Match contra entrenamiento"), tags$p("Poné en relación el promedio móvil del partido con el acumulado de las sesiones elegidas.")),
          tags$div(class = "lift-analysis-grid", tags$aside(class = "lift-filter-panel", lift_filter_rail("Filtros", "Construir la semana", "Elegí jugadores, fechas de entrenamiento y la métrica de referencia.", c("jugador_micro", "puesto_micro", "tarea_micro", "duracion_micro"), "metricas_microciclo", "Métricas", tagList(tags$div(class = "lift-field", sliderInput("ventana_movil_micro", "MD rolling average", min = 3, max = 10, value = 5, step = 1)), tags$div(class = "lift-field", uiOutput("selector_fechas_entreno_micro"))))), tags$section(class = "lift-output-panel", tags$div(class = "lift-threshold-dock", tags$span(class = "lift-control-label", "Umbrales del ratio"), tags$p("Ajustalos por métrica cuando el contexto competitivo lo requiera."), uiOutput("umbral_ratio_microciclo_ui")), tags$div(class = "lift-chart-stack", uiOutput("microciclo_ratio_plot_ui"))))
        ),
        tabPanel(
          title = tagList(tags$i(class = "bi bi-grid-3x3-gap-fill", `aria-hidden` = "true"), tags$span(class = "lift-tab-label", "Cuadrante")),
          tags$div(class = "lift-tab-intro", tags$span(class = "lift-eyebrow", "Perfil"), tags$h1("Dos métricas, cuatro lecturas"), tags$p("Agregá por jugador y ubicá cada perfil respecto de la mediana de la sesión.")),
          tags$div(class = "lift-analysis-grid", tags$aside(class = "lift-filter-panel", lift_filter_rail("Filtros", "Definir el corte", "Filtrá la sesión y elegí exactamente dos métricas.", c("jugador_cuad", "puesto_cuad", "matchday_cuad", "tarea_cuad", "duracion_cuad"), "metricas_cuad", "Dos métricas", tags$div(class = "lift-field", uiOutput("filtro_sesion_cuad")))), tags$section(class = "lift-output-panel", tags$div(class = "lift-threshold-dock", tags$span(class = "lift-control-label", "Rangos de lectura"), tags$p("Reducí el rango si necesitás aislar valores operativos."), uiOutput("sliders_metricas_cuad")), tags$div(class = "lift-chart-stack", uiOutput("cuadrante_plot_ui"))))
        )
      )
    )
  )
)
