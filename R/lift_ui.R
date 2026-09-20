# Comunidad LIFT / GPS workspace UI ---------------------------------------

lift_asset_path <- function(filename) {
  roots <- c(
    if (exists("gps_app_root", inherits = TRUE)) get("gps_app_root", inherits = TRUE) else character(0),
    getwd()
  )
  candidates <- unique(file.path(roots[nzchar(roots)], "www", filename))
  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0) existing[[1]] else filename
}

lift_data_uri <- function(filename, mime) {
  path <- lift_asset_path(filename)
  if (file.exists(path) && requireNamespace("base64enc", quietly = TRUE)) {
    return(base64enc::dataURI(file = path, mime = mime))
  }
  path
}

lift_logo_src <- lift_data_uri("isologo.png", "image/png")
lift_css_path <- lift_asset_path("lift-theme.css")
lift_css_tag <- if (file.exists(lift_css_path)) {
  htmltools::includeCSS(lift_css_path)
} else {
  tags$link(rel = "stylesheet", href = "lift-theme.css")
}

lift_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = NULL,
  bg = "#0A0A0B",
  fg = "#F4F4F1",
  primary = "#FF003D",
  secondary = "#AEB4B8",
  base_font = bslib::font_google("IBM Plex Sans"),
  heading_font = bslib::font_google("Barlow Condensed")
)

lift_select <- function(id, label, multiple = TRUE, choices = NULL, width = "100%") {
  selectizeInput(
    id, label, choices = choices, multiple = multiple, width = width,
    options = list(placeholder = "Seleccioná una opción", `closeAfterSelect` = !multiple)
  )
}

lift_view_header <- function(kicker, title, description) {
  tags$header(
    class = "lift-view-header",
    tags$span(class = "lift-eyebrow", kicker),
    tags$h1(title),
    tags$p(description)
  )
}

lift_filter_panel <- function(title, description, controls, class = "") {
  tags$aside(
    class = paste("lift-filter-panel", class),
    tags$div(
      class = "lift-filter-panel-heading",
      tags$span(class = "lift-section-index", "CONTROLES"),
      tags$h2(title),
      tags$p(description)
    ),
    controls
  )
}

lift_chart_panel <- function(title, description, output_id, class = "") {
  tags$section(
    class = paste("lift-chart-panel", class),
    tags$div(
      class = "lift-panel-heading",
      tags$div(tags$span(class = "lift-section-index", "VISUAL"), tags$h2(title)),
      tags$p(description)
    ),
    plotly::plotlyOutput(output_id, height = "430px")
  )
}

lift_nav_choices <- c(
  "Cargar datos" = "source",
  "Overview" = "overview",
  "Tendencia" = "trend",
  "Match day" = "match",
  "Tareas" = "task",
  "Sesión" = "session",
  "Readiness" = "readiness",
  "Microciclo" = "microcycle",
  "Perfil" = "profile",
  "Datos crudos" = "data"
)

ui <- fluidPage(
  theme = lift_theme,
  tags$head(
    tags$title("GPS LIFT | Comunidad LIFT"),
    tags$meta(name = "description", content = "Workspace de análisis GPS para sports scientists de Comunidad LIFT."),
    tags$meta(name = "theme-color", content = "#0A0A0B"),
    tags$link(rel = "icon", type = "image/png", href = lift_logo_src),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"),
    lift_css_tag,
    tags$script(HTML("document.documentElement.lang = 'es';")),
    tags$script(HTML("(
      function () {
        document.addEventListener('click', function (event) {
          if (event.target.closest('#lift-mobile-menu')) {
            document.body.classList.toggle('lift-sidebar-open');
          }
        });
        document.addEventListener('change', function (event) {
          if (event.target.closest('#main_nav')) {
            document.body.classList.remove('lift-sidebar-open');
          }
        });
      }
    )();"))
  ),

  tags$a(class = "lift-skip-link", href = "#lift-workspace", "Saltar al contenido"),

  tags$header(
    class = "lift-appbar",
    tags$a(
      class = "lift-brand", href = "#lift-workspace",
      tags$img(src = lift_logo_src, alt = "Comunidad LIFT", class = "lift-brand-mark"),
      tags$span(class = "lift-brand-copy", tags$strong("GPS LIFT"), tags$small("Performance workspace"))
    ),
    tags$div(class = "lift-appbar-context", tags$span(class = "lift-system-label", "DATA / LOAD / DECISION"), tags$div(class = "lift-status-chip", tags$span(class = "lift-status-dot"), uiOutput("data_status"))),
    tags$button(id = "lift-mobile-menu", class = "lift-mobile-menu", type = "button", `aria-label` = "Abrir navegación", tags$i(class = "bi bi-list", `aria-hidden` = "true"))
  ),

  tags$div(
    class = "lift-app-shell",
    tags$aside(
      id = "lift-sidebar", class = "lift-sidebar",
      tags$div(class = "lift-sidebar-heading", tags$span(class = "lift-eyebrow", "WORKSPACE"), tags$h2("GPS / control"), tags$p("Una vista activa, una pregunta concreta.")),
      tags$div(class = "lift-sidebar-state", tags$span(class = "lift-section-index", "BASE ACTIVA"), uiOutput("sidebar_state")),
      tags$div(class = "lift-sidebar-nav-label", "Navegación"),
      radioButtons("main_nav", label = NULL, choices = lift_nav_choices, selected = "source"),
      tags$div(class = "lift-sidebar-foot", tags$span(class = "lift-section-index", "LIFT / GPS"), tags$small("Catapult · WIMU · Stats Sports"))
    ),

      tags$main(id = "lift-workspace", class = "lift-main",
      bslib::navset_pill(
        id = "main_tabs",

        bslib::nav_panel(
          title = "Cargar datos", value = "source",
          lift_view_header("01 / INGESTA", "Traé los datos al workspace", "Importá un export, confirmá su semántica y recién después empezá a leer gráficos."),
          tags$div(class = "lift-source-grid",
            tags$article(class = "lift-source-card lift-source-card-primary",
              tags$div(class = "lift-card-kicker", tags$i(class = "bi bi-cloud-arrow-down", `aria-hidden` = "true"), "FUENTE CONECTADA"),
              textInput("google_sheet_url", "Google Sheet URL o ID", placeholder = "Pegá una URL o un ID de spreadsheet"),
              actionButton("load_google_sheet", "Cargar Google Sheet", icon = icon("arrow-right"), class = "lift-button lift-button-primary"),
              tags$small(class = "lift-helper-copy", "La hoja debe ser accesible como export CSV desde docs.google.com.")
            ),
            tags$article(class = "lift-source-card lift-file-card",
              tags$div(class = "lift-card-kicker", tags$i(class = "bi bi-file-earmark-bar-graph", `aria-hidden` = "true"), "ARCHIVOS LOCALES"),
              fileInput("file", "CSV, XLS/XLSX o JSON", multiple = TRUE, accept = c(".csv", ".xls", ".xlsx", ".json"), buttonLabel = "Elegir archivos", placeholder = "Seleccioná uno o varios exports"),
              tags$small(class = "lift-helper-copy", "Se aceptan exports grandes; el origen y el proveedor quedan trazables.")
            ),
            tags$article(class = "lift-source-card lift-source-card-status",
              tags$div(class = "lift-card-kicker", tags$i(class = "bi bi-activity", `aria-hidden` = "true"), "ESTADO DE LA BASE"),
              uiOutput("source_manifest"),
              actionButton("reset_base", "Vaciar workspace", icon = icon("trash"), class = "lift-button lift-button-quiet"),
              tags$small(class = "lift-helper-copy", "El reset afecta únicamente la sesión local actual.")
            )
          ),
          tags$section(class = "lift-mapping-card",
            tags$div(class = "lift-mapping-card-heading", tags$div(tags$span(class = "lift-section-index", "02 / SEMÁNTICA"), tags$h2("Mapeá una vez, reutilizá en todas las vistas"), tags$p("Los gráficos se activan cuando aplicás jugador, fecha y métricas. El resto del contexto mejora la lectura."))),
            tags$div(class = "lift-mapping-grid",
              lift_select("player_col", "Jugador", multiple = FALSE),
              lift_select("position_col", "Puesto / grupo", multiple = FALSE),
              lift_select("matchday_col", "Match day / sesión", multiple = FALSE),
              lift_select("task_col", "Tarea / drill", multiple = FALSE),
              lift_select("session_type_col", "Tipo de sesión", multiple = FALSE),
              lift_select("date_col", "Fecha", multiple = FALSE),
              lift_select("duration_col", "Duración", multiple = FALSE),
              lift_select("start_col", "Hora de inicio", multiple = FALSE),
              lift_select("end_col", "Hora de fin", multiple = FALSE),
              lift_select("metric_col", "Métricas de trabajo", multiple = TRUE)
            ),
            tags$div(class = "lift-mapping-actions", uiOutput("mapping_notes"), actionButton("apply_mapping", "Aplicar mapeo y abrir Overview", icon = icon("check"), class = "lift-button lift-button-primary"))
          )
        ),

        bslib::nav_panel(
          title = "Overview", value = "overview",
          lift_view_header("03 / OVERVIEW", "Lo importante, primero", "Un resumen de la base activa para decidir dónde profundizar."),
          tags$div(class = "lift-overview-controls", tags$div(class = "lift-control-title", tags$span(class = "lift-section-index", "POBLACIÓN"), tags$strong("Definí el corte de lectura")), lift_select("metric_overview", "Métrica principal", multiple = FALSE), lift_select("player_overview", "Jugadores"), dateRangeInput("date_overview", "Período", start = Sys.Date() - 28, end = Sys.Date(), separator = " → ")),
          uiOutput("overview_kpis"),
          tags$div(class = "lift-overview-grid", lift_chart_panel("Evolución de la carga", "Promedio diario de la métrica principal, con el tipo de sesión como contexto.", "overview_plot", "lift-overview-main"), tags$section(class = "lift-insight-panel", tags$div(class = "lift-panel-heading", tags$div(tags$span(class = "lift-section-index", "ATENCIÓN"), tags$h2("Puntos para revisar")), tags$p("Señales descriptivas; no sustituyen el criterio del staff.")), uiOutput("overview_attention")), tags$section(class = "lift-table-panel", tags$div(class = "lift-panel-heading", tags$div(tags$span(class = "lift-section-index", "ÚLTIMA SESIÓN"), tags$h2("Snapshot por jugador")), uiOutput("overview_table"))))
        ),

        bslib::nav_panel(
          title = "Tendencia", value = "trend",
          lift_view_header("04 / HISTORIA", "¿Cómo está evolucionando la carga?", "Seguí una métrica, filtrá el tipo de sesión y compará días equivalentes."),
          tags$div(class = "lift-analysis-grid", lift_filter_panel("Construir la serie", "Filtros de población para comparar días equivalentes.", tagList(lift_select("metric_trend", "Métrica", FALSE), lift_select("player_trend", "Jugadores"), lift_select("session_type_trend", "Tipo de sesión"), dateRangeInput("date_trend", "Período", start = Sys.Date() - 28, end = Sys.Date(), separator = " → "))), lift_chart_panel("Carga diaria", "Una línea por grupo de sesión; la tabla conserva el detalle diario.", "trend_plot"), tags$section(class = "lift-table-panel", tags$div(class = "lift-panel-heading", tags$h2("Lectura diaria")), uiOutput("trend_table")))
        ),

        bslib::nav_panel(
          title = "Match day", value = "match",
          lift_view_header("05 / DISTRIBUCIÓN", "¿Qué rango de carga está sosteniendo el grupo?", "Compará match days y detectá dispersiones que merecen una revisión individual."),
          tags$div(class = "lift-analysis-grid", lift_filter_panel("Aislar la comparación", "No mezcles tareas o grupos si la pregunta es competitiva.", tagList(lift_select("metric_match", "Métrica", FALSE), lift_select("player_match", "Jugadores"), lift_select("task_match", "Tareas"))), lift_chart_panel("Distribución por match day", "Mediana, rango intercuartílico y observaciones individuales.", "matchday_plot"), tags$section(class = "lift-table-panel", tags$div(class = "lift-panel-heading", tags$h2("Resumen de distribución")), uiOutput("matchday_table")))
        ),

        bslib::nav_panel(
          title = "Tareas", value = "task",
          lift_view_header("06 / CONTEXTO", "¿Qué estímulo está explicando la carga?", "Separá campo, gimnasio, recuperación y drills para no atribuir el mismo estímulo a contextos distintos."),
          tags$div(class = "lift-analysis-grid", lift_filter_panel("Comparar estímulos", "Usá la tarea como dimensión principal y el jugador como corte opcional.", tagList(lift_select("metric_task", "Métrica", FALSE), lift_select("task_task", "Tareas"), lift_select("player_task", "Jugadores"), dateRangeInput("date_task", "Período", start = Sys.Date() - 28, end = Sys.Date(), separator = " → "))), lift_chart_panel("Carga por tarea", "Comparación de valores y dispersión entre estímulos.", "task_plot"), tags$section(class = "lift-table-panel", tags$div(class = "lift-panel-heading", tags$h2("Resumen por tarea")), uiOutput("task_table")))
        ),

        bslib::nav_panel(
          title = "Sesión", value = "session",
          lift_view_header("07 / INFORME", "¿Qué pasó hoy?", "Elegí una fecha para obtener una lectura operativa por jugador, con valores exactos y contexto."),
          tags$div(class = "lift-analysis-grid", lift_filter_panel("Seleccionar sesión", "La sesión es el objeto de trabajo; el gráfico ordena el grupo.", tagList(dateInput("session_date", "Fecha", value = Sys.Date()), lift_select("metric_session", "Métrica", FALSE), lift_select("task_session", "Tareas"))), lift_chart_panel("Perfil de la sesión", "Ranking descriptivo del valor de cada jugador; no es un ranking de calidad sin contexto.", "session_plot"), tags$section(class = "lift-table-panel", tags$div(class = "lift-panel-heading", tags$h2("Detalle por jugador")), uiOutput("session_table")))
        ),

        bslib::nav_panel(
          title = "Readiness", value = "readiness",
          lift_view_header("08 / REFERENCIA", "¿Quién se está alejando de su historia?", "El z-score y el ACWR muestran desvíos descriptivos. La interpretación final requiere disponibilidad y contexto."),
          tags$div(class = "lift-analysis-grid", lift_filter_panel("Ajustar la referencia", "Usá suficiente historia y revisá las bandas antes de interpretar.", tagList(lift_select("metric_readiness", "Métrica", FALSE), sliderInput("z_window", "Historia previa (sesiones)", min = 3, max = 10, value = 5, step = 1), sliderInput("acwr_acute", "Media vida aguda (días)", min = 3, max = 14, value = 7, step = 1), sliderInput("acwr_chronic", "Media vida crónica (días)", min = 14, max = 42, value = 28, step = 1))), tags$div(class = "lift-chart-stack", lift_chart_panel("Desvío individual", "Último z-score disponible por jugador.", "readiness_z_plot"), lift_chart_panel("Carga aguda / crónica", "Relación EWMA; no debe leerse como diagnóstico.", "readiness_acwr_plot")), tags$section(class = "lift-table-panel", tags$div(class = "lift-panel-heading", tags$h2("Jugadores para contextualizar")), uiOutput("readiness_table")))
        ),

        bslib::nav_panel(
          title = "Microciclo", value = "microcycle",
          lift_view_header("09 / PLAN", "¿Cómo se relacionan partido y entrenamiento?", "Compará el promedio de los partidos recientes con la carga acumulada previa al ancla elegida."),
          tags$div(class = "lift-analysis-grid", lift_filter_panel("Construir la semana", "El ratio es partido promedio dividido por entrenamiento acumulado.", tagList(dateInput("micro_anchor", "Partido de referencia", value = Sys.Date()), lift_select("metric_micro", "Métrica", FALSE), sliderInput("micro_match_window", "Partidos incluidos", min = 1, max = 5, value = 3, step = 1), sliderInput("micro_training_days", "Días de entrenamiento", min = 3, max = 14, value = 7, step = 1))), lift_chart_panel("Match / training ratio", "Ratio descriptivo por jugador; los ceros y faltantes quedan como no disponible.", "microcycle_plot"), tags$section(class = "lift-table-panel", tags$div(class = "lift-panel-heading", tags$h2("Detalle del microciclo")), uiOutput("microcycle_table")))
        ),

        bslib::nav_panel(
          title = "Perfil", value = "profile",
          lift_view_header("10 / RELACIÓN", "¿Qué combinación de métricas define el perfil?", "Ubicá jugadores respecto de la mediana de una sesión. La mediana divide perfiles; no define objetivos clínicos."),
          tags$div(class = "lift-analysis-grid", lift_filter_panel("Elegir dos señales", "Usá magnitudes compatibles y leé los cuadrantes como clasificación descriptiva.", tagList(dateInput("profile_date", "Fecha", value = Sys.Date()), lift_select("profile_x", "Métrica X", FALSE), lift_select("profile_y", "Métrica Y", FALSE))), lift_chart_panel("Matriz de perfil", "Cada punto es un jugador; las líneas son medianas de la población visible.", "quadrant_plot"), tags$section(class = "lift-table-panel", tags$div(class = "lift-panel-heading", tags$h2("Jugadores por cuadrante")), uiOutput("quadrant_table")))
        ),

        bslib::nav_panel(
          title = "Datos crudos", value = "data",
          lift_view_header("11 / TRAZABILIDAD", "La fuente antes de exportar", "Inspeccioná filas, proveedor, fecha y métricas. Lo que exportás es lo que la base conserva."),
          tags$div(class = "lift-data-actions", downloadButton("download_filtered", "Descargar base visible", class = "lift-button lift-button-primary")),
          tags$div(class = "lift-data-view", tags$div(class = "lift-table-frame", DT::DTOutput("data_table")))
  )
)
)
)
)
