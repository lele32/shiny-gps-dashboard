# Comunidad LIFT / GPS server ---------------------------------------------

lift_palette <- list(
  red = "#FF003D",
  red_soft = "#FF5B79",
  white = "#F4F4F1",
  gray = "#AEB4B8",
  gray_dark = "#596166",
  panel = "#151719",
  line = "rgba(244,244,241,0.16)"
)

lift_empty_plot <- function(title = "Sin datos visibles", message = "Cargá una base o ajustá los filtros.") {
  plotly::plotly_empty(type = "scatter", mode = "markers") %>%
    plotly::layout(
      title = list(text = paste0("<b>", title, "</b><br><sup>", message, "</sup>"), x = 0.02, xanchor = "left"),
      plot_bgcolor = "transparent", paper_bgcolor = "transparent",
      font = list(color = lift_palette$white, family = "IBM Plex Sans"),
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      margin = list(l = 20, r = 20, t = 58, b = 20)
    ) %>%
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

lift_theme_gg <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "IBM Plex Sans") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = "#303438", linewidth = 0.25),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = lift_palette$gray),
      axis.title = ggplot2::element_text(colour = lift_palette$gray, face = "bold"),
      plot.title = ggplot2::element_text(colour = lift_palette$white, family = "Barlow Condensed", face = "bold", size = 18, hjust = 0),
      plot.subtitle = ggplot2::element_text(colour = lift_palette$gray, size = 10, hjust = 0),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(colour = lift_palette$gray),
      legend.title = ggplot2::element_text(colour = lift_palette$white, face = "bold")
    )
}

lift_plotly <- function(plot, tooltip = "text", legend = FALSE) {
  plotly::ggplotly(plot, tooltip = tooltip) %>%
    plotly::layout(
      plot_bgcolor = "transparent", paper_bgcolor = "transparent",
      font = list(color = lift_palette$white, family = "IBM Plex Sans"),
      margin = list(l = 48, r = 18, t = 24, b = 56),
      hoverlabel = list(bgcolor = lift_palette$panel, bordercolor = lift_palette$red, font = list(color = lift_palette$white)),
      showlegend = legend
    ) %>%
    plotly::config(displaylogo = FALSE, responsive = TRUE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
}

lift_value <- function(x, digits = 1) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) "—" else formatC(x, format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
}

lift_html_escape <- function(x) htmltools::htmlEscape(as.character(x), attribute = FALSE)

lift_table_ui <- function(id) DT::DTOutput(id)

lift_visible_data <- function(data) {
  internal <- c(".gps_player", ".gps_position", ".gps_matchday", ".gps_task", ".gps_date",
                ".gps_session_type", ".gps_duration_min")
  data[, setdiff(names(data), internal), drop = FALSE]
}

lift_empty_table <- function(message = "Sin registros para mostrar") {
  DT::datatable(data.frame(Estado = message, check.names = FALSE), rownames = FALSE, options = list(dom = "t"))
}

lift_kpi_cards <- function(cards) {
  tags$div(class = "lift-kpi-strip", lapply(cards, function(card) {
    tags$div(class = "lift-kpi-card", tags$small(card$label), tags$strong(card$value), tags$span(card$note))
  }))
}

lift_filter_values <- function(data, player = NULL, position = NULL, matchday = NULL, task = NULL,
                               session_type = NULL, date_range = NULL, duration_range = NULL) {
  gps_apply_filters(
    data, player = player, position = position, matchday = matchday, task = task,
    session_type = session_type, date_range = date_range, duration_range = duration_range
  )
}

gps_server <- function(input, output, session) {
  base_data <- shiny::reactiveVal(NULL)
  applied_mapping <- shiny::reactiveVal(NULL)
  applied_metrics <- shiny::reactiveVal(NULL)

  mapping_inputs <- shiny::reactive({
    list(
      player = input$player_col,
      position = input$position_col,
      matchday = input$matchday_col,
      task = input$task_col,
      session_type = input$session_type_col,
      date = input$date_col,
      duration = input$duration_col,
      start = input$start_col,
      end = input$end_col
    )
  })

  mapping <- shiny::reactive({
    applied_mapping() %||% mapping_inputs()
  })

  context_data <- shiny::reactive({
    shiny::req(base_data())
    gps_derive_context(base_data(), mapping())
  })

  metric_choices <- shiny::reactive({
    shiny::req(base_data())
    metrics <- gps_metric_candidates(base_data())
    mapped <- applied_metrics()
    if (!is.null(mapped) && length(mapped) > 0) {
      selected <- intersect(as.character(mapped), metrics)
      if (length(selected) > 0) metrics <- selected
    }
    metrics
  })

  selected_metric <- function(id, fallback = NULL) {
    value <- input[[id]]
    if (is.null(value) || length(value) == 0 || !nzchar(as.character(value[[1]]))) fallback else as.character(value[[1]])
  }

  selected_filter <- function(id) {
    value <- input[[id]]
    if (is.null(value) || length(value) == 0) NULL else value
  }

  guess_mapping <- function(data) {
    columns <- names(data)
    guessed <- list(
      player_col = gps_guess_column(columns, "player"),
      position_col = gps_guess_column(columns, "position"),
      matchday_col = gps_guess_column(columns, "matchday"),
      task_col = gps_guess_column(columns, "task"),
      session_type_col = gps_guess_column(columns, "session_type"),
      date_col = gps_guess_column(columns, "date"),
      duration_col = gps_guess_column(columns, "duration"),
      start_col = gps_guess_column(columns, "start"),
      end_col = gps_guess_column(columns, "end")
    )
    list(
      mapping = list(
        player = guessed$player_col,
        position = guessed$position_col,
        matchday = guessed$matchday_col,
        task = guessed$task_col,
        session_type = guessed$session_type_col,
        date = guessed$date_col,
        duration = guessed$duration_col %||% "None",
        start = guessed$start_col %||% "None",
        end = guessed$end_col %||% "None"
      ),
      metrics = head(gps_metric_candidates(data), 3L),
      columns = columns,
      guessed = guessed
    )
  }

  update_mapping_inputs <- function(data, guessed = guess_mapping(data)) {
    columns <- guessed$columns
    for (id in names(guessed$guessed)) {
      value <- guessed$guessed[[id]]
      if (id %in% c("duration_col", "start_col", "end_col")) {
        updateSelectizeInput(session, id, choices = c("None", columns), selected = value %||% "None")
      } else {
        updateSelectizeInput(session, id, choices = columns, selected = value %||% character(0))
      }
    }
    updateSelectizeInput(session, "metric_col", choices = gps_metric_candidates(data), selected = guessed$metrics)
    invisible(guessed)
  }

  append_import <- function(data_new, source_type, source_name) {
    if (!is.data.frame(data_new) || nrow(data_new) == 0) {
      showNotification("La fuente no contiene una tabla con filas utilizables.", type = "error", duration = 6)
      return(invisible(FALSE))
    }
    names(data_new) <- make.unique(trimws(names(data_new)))
    data_new <- gps_deduplicate_rows(data_new)
    data_new$.gps_source_type <- source_type
    data_new$.gps_source_name <- source_name
    data_new$.gps_provider <- gps_detect_provider(source_name, data_new)
    data_new$.gps_source_row <- seq_len(nrow(data_new))
    combined <- if (is.null(base_data())) data_new else dplyr::bind_rows(base_data(), data_new)
    base_data(gps_deduplicate_rows(combined))
    guessed <- guess_mapping(base_data())
    update_mapping_inputs(base_data(), guessed)
    applied_mapping(guessed$mapping)
    applied_metrics(guessed$metrics)
    showNotification(paste0(nrow(data_new), " filas agregadas desde ", source_name, "."), type = "message", duration = 5)
    invisible(TRUE)
  }

  read_local_files <- function(file_input) {
    if (is.null(file_input) || nrow(file_input) == 0) return(NULL)
    if (any(file_input$size > 100 * 1024^2, na.rm = TRUE)) stop("Cada archivo debe pesar menos de 100 MB.", call. = FALSE)
    data_list <- lapply(seq_len(nrow(file_input)), function(i) {
      data_i <- gps_read_source_file(file_input$datapath[[i]], tools::file_ext(file_input$name[[i]]))
      data_i
    })
    dplyr::bind_rows(data_list)
  }

  observeEvent(input$file, {
    data_new <- tryCatch(read_local_files(input$file), error = function(error) {
      showNotification(paste("No se pudo leer el archivo:", error$message), type = "error", duration = 8)
      NULL
    })
    if (!is.null(data_new)) append_import(data_new, "file", paste(input$file$name, collapse = ", "))
  }, ignoreInit = TRUE)

  observeEvent(input$load_google_sheet, {
    raw_url <- trimws(input$google_sheet_url %||% "")
    is_id <- grepl("^[A-Za-z0-9_-]{10,}$", raw_url)
    is_url <- grepl("^https?://docs\\.google\\.com/spreadsheets/d/[A-Za-z0-9_-]+", raw_url)
    if (!is_id && !is_url) {
      showNotification("Pegá una URL válida de Google Sheets o el ID del spreadsheet.", type = "warning", duration = 6)
      return()
    }
    sheet_id <- if (is_id) raw_url else sub("^https?://docs\\.google\\.com/spreadsheets/d/([^/]+).*$", "\\1", raw_url)
    export_url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv")
    data_new <- tryCatch(
      readr::read_csv(export_url, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE, progress = FALSE),
      error = function(error) {
        showNotification(paste("No se pudo leer Google Sheets:", error$message), type = "error", duration = 8)
        NULL
      }
    )
    if (!is.null(data_new)) append_import(data_new, "google_sheet", raw_url)
  }, ignoreInit = TRUE)

  observeEvent(input$reset_base, {
    showModal(modalDialog(
      title = "Vaciar workspace",
      "Se eliminarán los datos cargados de esta sesión local. Los archivos originales no se modifican.",
      footer = tagList(modalButton("Cancelar"), actionButton("confirm_reset", "Vaciar base", class = "lift-button lift-button-quiet")),
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_reset, {
    base_data(NULL)
    applied_mapping(NULL)
    applied_metrics(NULL)
    removeModal()
    showNotification("Workspace vacío. Podés cargar una nueva base.", type = "message", duration = 4)
  }, ignoreInit = TRUE)

  observeEvent(input$apply_mapping, {
    data <- base_data()
    if (is.null(data) || nrow(data) == 0) {
      showNotification("Primero cargá un archivo para poder aplicar el mapeo.", type = "warning", duration = 6)
      return()
    }
    current <- mapping_inputs()
    required <- current[c("player", "date")]
    missing <- names(required)[vapply(required, function(x) {
      is.null(x) || length(x) == 0 || is.na(x[[1]]) || !nzchar(as.character(x[[1]])) || identical(as.character(x[[1]]), "None")
    }, logical(1))]
    if (length(missing) > 0) {
      showNotification(paste0("Completá el mapeo de ", paste(missing, collapse = " y "), " antes de abrir Overview."), type = "warning", duration = 7)
      return()
    }
    metrics <- gps_metric_candidates(data)
    selected <- intersect(as.character(input$metric_col %||% character(0)), metrics)
    if (length(selected) == 0) selected <- head(metrics, 3L)
    applied_mapping(current)
    applied_metrics(selected)
    bslib::nav_select("main_tabs", selected = "overview", session = session)
    updateRadioButtons(session, "main_nav", selected = "overview")
    showNotification("Mapeo aplicado. Las vistas ya trabajan con este contexto.", type = "message", duration = 4)
  }, ignoreInit = TRUE)

  observeEvent(input$main_nav, {
    bslib::nav_select("main_tabs", selected = input$main_nav, session = session)
  }, ignoreInit = FALSE)

  observeEvent(input$main_tabs, {
    if (!is.null(input$main_tabs)) updateRadioButtons(session, "main_nav", selected = input$main_tabs)
  }, ignoreInit = TRUE)

  output$data_status <- renderUI({
    data <- base_data()
    if (is.null(data) || nrow(data) == 0) return(tags$span("Sin datos"))
    tags$span(paste0(formatC(nrow(data), format = "d", big.mark = ".", decimal.mark = ","), " filas"))
  })

  output$sidebar_state <- renderUI({
    data <- base_data()
    if (is.null(data) || nrow(data) == 0) {
      return(tags$div(class = "lift-sidebar-empty", tags$strong("Sin datos"), tags$span("Esperando un export")))
    }
    context <- gps_derive_context(data, mapping())
    players <- sum(!duplicated(context$.gps_player) & !is.na(context$.gps_player) & nzchar(context$.gps_player))
    dates <- context$.gps_date[!is.na(context$.gps_date)]
    latest <- if (length(dates) > 0) format(max(dates), "%d %b %Y") else "Fecha pendiente"
    tags$div(class = "lift-sidebar-metrics", tags$strong(paste0(formatC(nrow(data), format = "d", big.mark = ".", decimal.mark = ","), " filas")), tags$span(paste(players, "jugadores")), tags$small(paste("Última fecha", latest)))
  })

  output$source_manifest <- renderUI({
    data <- base_data()
    if (is.null(data) || nrow(data) == 0) {
      return(tags$div(class = "lift-manifest-empty", tags$strong("Workspace vacío"), tags$span("Esperando un export")))
    }
    context <- gps_derive_context(data, mapping())
    players <- if (".gps_player" %in% names(context)) {
      length(unique(context$.gps_player[!is.na(context$.gps_player) & nzchar(context$.gps_player)]))
    } else {
      0L
    }
    providers <- unique(data$.gps_provider %||% "unknown")
    sources <- unique(data$.gps_source_name %||% "source")
    tags$div(class = "lift-manifest", tags$strong(paste0(formatC(nrow(data), format = "d", big.mark = ".", decimal.mark = ","), " filas")), tags$span(paste(players, "jugadores")), tags$span(paste(length(sources), "fuente(s)")), tags$small(paste(providers, collapse = " · ")))
  })

  output$data_empty_state <- renderUI({
    if (!is.null(base_data()) && nrow(base_data()) > 0) return(NULL)
    tags$section(class = "lift-empty-state", tags$div(class = "lift-empty-index", "WORKSPACE / READY"), tags$div(tags$h2("Cargá un export para abrir el dashboard"), tags$p("La base está vacía. Empezá por un CSV, XLSX, JSON o Google Sheet; después confirmá el mapeo y el overview se activa solo."), tags$div(class = "lift-empty-steps", tags$span("01 / importar"), tags$span("02 / mapear"), tags$span("03 / interpretar"))))
  })

  output$mapping_notes <- renderUI({
    data <- base_data()
    if (is.null(data)) return(tags$p(class = "lift-helper-copy", "Cargá una fuente para habilitar el mapeo."))
    current <- mapping_inputs()
    required <- current[c("player", "date")]
    missing <- names(required)[vapply(required, function(x) {
      is.null(x) || length(x) == 0 || is.na(x[[1]]) || !nzchar(as.character(x[[1]])) || identical(as.character(x[[1]]), "None")
    }, logical(1))]
    candidates <- gps_metric_candidates(data)
    selected <- intersect(as.character(input$metric_col %||% character(0)), candidates)
    metrics <- metric_choices()
    if (length(missing) > 0) {
      tags$div(class = "lift-mapping-note is-warning", tags$strong("Falta completar: "), paste(missing, collapse = " y "), tags$span(". Sin jugador y fecha, las vistas históricas no son interpretables."))
    } else if (is.null(applied_mapping()) || !identical(current, applied_mapping()) || !identical(selected, applied_metrics())) {
      tags$div(class = "lift-mapping-note is-pending", tags$strong("Cambios sin aplicar"), tags$span(". Confirmá el mapeo para actualizar todas las vistas."))
    } else {
      tags$div(class = "lift-mapping-note", tags$strong(length(metrics), " métricas activas"), tags$span(". Mapeo aplicado; las unidades se infieren del nombre de columna."))
    }
  })

  observe({
    shiny::req(base_data())
    data <- context_data()
    values <- function(column) sort(unique(as.character(data[[column]][!is.na(data[[column]]) & nzchar(as.character(data[[column]]))])))
    for (id in c("player_overview", "player_trend", "player_match", "player_task")) updateSelectizeInput(session, id, choices = values(".gps_player"))
    updateSelectizeInput(session, "task_match", choices = values(".gps_task"))
    updateSelectizeInput(session, "task_task", choices = values(".gps_task"))
    updateSelectizeInput(session, "task_session", choices = values(".gps_task"))
    updateSelectizeInput(session, "session_type_trend", choices = c("match", "training", "unknown"))
    metrics <- metric_choices()
    for (id in c("metric_overview", "metric_trend", "metric_match", "metric_task", "metric_session", "metric_readiness", "metric_micro", "profile_x", "profile_y")) {
      updateSelectizeInput(session, id, choices = metrics, selected = if (length(metrics) > 0) metrics[[1]] else character(0))
    }
    if (length(metrics) > 1) updateSelectizeInput(session, "profile_y", selected = metrics[[2]])
    valid_dates <- data$.gps_date[!is.na(data$.gps_date)]
    if (length(valid_dates) > 0) {
      date_min <- min(valid_dates)
      date_max <- max(valid_dates)
      for (id in c("date_overview", "date_trend", "date_task")) updateDateRangeInput(session, id, start = date_min, end = date_max, min = date_min, max = date_max)
      for (id in c("session_date", "micro_anchor", "profile_date")) updateDateInput(session, id, value = date_max, min = date_min, max = date_max)
    }
  })

  overview_data <- reactive({
    req(context_data(), input$metric_overview)
    lift_filter_values(context_data(), player = selected_filter("player_overview"), date_range = input$date_overview)
  })
  trend_data <- reactive({
    req(context_data(), input$metric_trend)
    lift_filter_values(context_data(), player = selected_filter("player_trend"), session_type = selected_filter("session_type_trend"), date_range = input$date_trend)
  })
  match_data <- reactive({
    req(context_data(), input$metric_match)
    lift_filter_values(context_data(), player = selected_filter("player_match"), task = selected_filter("task_match"))
  })
  task_data <- reactive({
    req(context_data(), input$metric_task)
    lift_filter_values(context_data(), player = selected_filter("player_task"), task = selected_filter("task_task"), date_range = input$date_task)
  })
  session_data <- reactive({
    req(context_data(), input$metric_session, input$session_date)
    lift_filter_values(context_data(), task = selected_filter("task_session"))
  })

  output$overview_kpis <- renderUI({
    req(overview_data(), input$metric_overview)
    data <- overview_data()
    metric <- input$metric_overview
    summary <- gps_overview_summary(data, metric)
    lift_kpi_cards(list(
      list(label = "Registros visibles", value = formatC(nrow(data), format = "d", big.mark = ".", decimal.mark = ","), note = "después de filtros"),
      list(label = "Jugadores", value = length(unique(data$.gps_player[!is.na(data$.gps_player)])), note = "población visible"),
      list(label = "Sesiones", value = summary$sessions, note = paste("métrica:", metric)),
      list(label = "Última fecha", value = ifelse(is.na(summary$latest), "—", format(summary$latest, "%d %b %Y")), note = paste("media", lift_value(summary$latest_mean))),
      list(label = "Unidad", value = gps_metric_unit(metric), note = paste("agregación:", gps_metric_aggregation(metric)))
    ))
  })

  output$overview_attention <- renderUI({
    req(overview_data(), input$metric_overview)
    data <- overview_data()
    metric <- input$metric_overview
    snapshot <- gps_readiness_snapshot(data, metric, input$z_window %||% 5L)
    high <- sum(snapshot$band == "high", na.rm = TRUE)
    low <- sum(snapshot$band == "low", na.rm = TRUE)
    missing_history <- sum(snapshot$band == "not enough history", na.rm = TRUE)
    duration_coverage <- if (nrow(data) > 0) mean(is.finite(data$.gps_duration_min)) else 0
    tags$div(class = "lift-attention-list",
      tags$div(class = if (high > 0) "lift-attention-item" else "lift-attention-item is-neutral", tags$div(tags$strong(if (high > 0) paste(high, "jugador(es) con desvío alto") else "Sin desvíos altos visibles"), tags$span("z ≥ 1,5 respecto de la historia seleccionada."))),
      tags$div(class = if (low > 0) "lift-attention-item" else "lift-attention-item is-neutral", tags$div(tags$strong(if (low > 0) paste(low, "jugador(es) con desvío bajo") else "Sin desvíos bajos visibles"), tags$span("Revisar junto con disponibilidad y contenido de la sesión."))),
      tags$div(class = "lift-attention-item is-neutral", tags$div(tags$strong(if (missing_history > 0) paste(missing_history, "sin historia suficiente") else "Historia disponible"), tags$span("Un z-score no debe interpretarse sin una ventana previa completa."))),
      tags$div(class = "lift-attention-item is-neutral", tags$div(tags$strong(paste0(round(duration_coverage * 100), "% con duración")), tags$span("La duración mejora la lectura de carga relativa; revisá el mapeo si es baja.")))
    )
  })

  output$overview_table <- renderUI(lift_table_ui("overview_table_dt"))
  output$overview_table_dt <- renderDT({
    req(overview_data(), input$metric_overview)
    daily <- gps_daily_metric(overview_data(), input$metric_overview)
    if (nrow(daily) == 0) return(lift_empty_table())
    latest <- max(daily$date)
    table <- daily %>% filter(date == latest) %>% transmute(Jugador = player, Valor = round(value, 2), Tipo = session_type, Duración = round(duration_min, 1)) %>% arrange(desc(Valor))
    datatable(table, rownames = FALSE, options = list(pageLength = 8, dom = "tp", scrollX = TRUE))
  })

  output$overview_plot <- renderPlotly({
    req(overview_data(), input$metric_overview)
    daily <- gps_daily_metric(overview_data(), input$metric_overview)
    if (nrow(daily) == 0) return(lift_empty_plot("Sin sesiones visibles"))
    plot_data <- daily %>% group_by(date, session_type) %>% summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>% mutate(text = paste0("Fecha: ", date, "<br>Tipo: ", session_type, "<br>Valor: ", round(value, 2)))
    p <- ggplot(plot_data, aes(date, value, colour = session_type, group = session_type, text = text)) + geom_line(linewidth = 0.9) + geom_point(size = 2) + scale_color_manual(values = c(match = lift_palette$red, training = lift_palette$white, unknown = lift_palette$gray_dark), drop = FALSE) + labs(x = NULL, y = input$metric_overview) + lift_theme_gg()
    lift_plotly(p, "text", legend = TRUE)
  })

  output$trend_table <- renderUI(lift_table_ui("trend_table_dt"))
  output$trend_table_dt <- renderDT({
    req(trend_data(), input$metric_trend)
    daily <- gps_daily_metric(trend_data(), input$metric_trend)
    if (nrow(daily) == 0) return(lift_empty_table())
    table <- daily %>% transmute(Fecha = date, Tipo = session_type, Valor = round(value, 2), Jugadores = player) %>% arrange(desc(Fecha), Tipo)
    datatable(table, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$trend_plot <- renderPlotly({
    req(trend_data(), input$metric_trend)
    daily <- gps_daily_metric(trend_data(), input$metric_trend)
    if (nrow(daily) == 0) return(lift_empty_plot("Sin tendencia visible"))
    plot_data <- daily %>% group_by(date, session_type) %>% summarise(value = mean(value), .groups = "drop") %>% mutate(text = paste0("Fecha: ", date, "<br>Tipo: ", session_type, "<br>Promedio: ", round(value, 2)))
    p <- ggplot(plot_data, aes(date, value, colour = session_type, group = session_type, text = text)) + geom_line(linewidth = 0.9) + geom_point(size = 2) + scale_color_manual(values = c(match = lift_palette$red, training = lift_palette$white, unknown = lift_palette$gray_dark), drop = FALSE) + labs(x = NULL, y = input$metric_trend) + lift_theme_gg()
    lift_plotly(p, "text", legend = TRUE)
  })

  output$matchday_table <- renderUI(lift_table_ui("matchday_table_dt"))
  output$matchday_table_dt <- renderDT({
    req(match_data(), input$metric_match)
    frame <- gps_matchday_frame(match_data(), input$metric_match)
    if (nrow(frame) == 0) return(lift_empty_table())
    table <- frame %>% group_by(MatchDay = matchday) %>% summarise(N = n(), Mediana = round(median(value), 2), P25 = round(stats::quantile(value, .25), 2), P75 = round(stats::quantile(value, .75), 2), .groups = "drop")
    datatable(table, rownames = FALSE, options = list(pageLength = 8, dom = "tp", scrollX = TRUE))
  })

  output$matchday_plot <- renderPlotly({
    req(match_data(), input$metric_match)
    frame <- gps_matchday_frame(match_data(), input$metric_match)
    if (nrow(frame) == 0) return(lift_empty_plot("No hay match day mapeado"))
    frame$text <- paste0("Jugador: ", frame$player, "<br>MD: ", frame$matchday, "<br>Valor: ", round(frame$value, 2))
    p <- ggplot(frame, aes(factor(matchday), value, fill = factor(matchday), text = text)) + geom_boxplot(alpha = .76, outlier.shape = NA) + geom_jitter(width = .12, alpha = .48, colour = lift_palette$white, size = 1.2) + scale_fill_manual(values = rep(lift_palette$red, length(unique(frame$matchday)))) + labs(x = NULL, y = input$metric_match) + lift_theme_gg() + theme(legend.position = "none")
    lift_plotly(p, "text")
  })

  output$task_table <- renderUI(lift_table_ui("task_table_dt"))
  output$task_table_dt <- renderDT({
    req(task_data(), input$metric_task)
    frame <- gps_metric_frame(task_data(), input$metric_task)
    if (nrow(frame) == 0) return(lift_empty_table())
    table <- frame %>% group_by(Tarea = task) %>% summarise(N = n(), Media = round(mean(value), 2), Mediana = round(median(value), 2), P75 = round(stats::quantile(value, .75), 2), .groups = "drop") %>% arrange(desc(Media))
    datatable(table, rownames = FALSE, options = list(pageLength = 8, dom = "tp", scrollX = TRUE))
  })

  output$task_plot <- renderPlotly({
    req(task_data(), input$metric_task)
    frame <- gps_metric_frame(task_data(), input$metric_task)
    frame <- frame[!is.na(frame$task) & nzchar(frame$task), , drop = FALSE]
    if (nrow(frame) == 0) return(lift_empty_plot("No hay tareas mapeadas"))
    frame$text <- paste0("Tarea: ", frame$task, "<br>Jugador: ", frame$player, "<br>Valor: ", round(frame$value, 2))
    p <- ggplot(frame, aes(stats::reorder(task, value, FUN = median), value, text = text)) + geom_boxplot(fill = lift_palette$red, alpha = .78, outlier.shape = NA) + geom_jitter(width = .12, alpha = .42, colour = lift_palette$white, size = 1.1) + coord_flip() + labs(x = NULL, y = input$metric_task) + lift_theme_gg()
    lift_plotly(p, "text")
  })

  session_frame <- reactive({
    req(context_data(), input$metric_session, input$session_date)
    data <- lift_filter_values(context_data(), task = selected_filter("task_session"))
    gps_session_table(data, input$metric_session, input$session_date)
  })

  output$session_table <- renderUI(lift_table_ui("session_table_dt"))
  output$session_table_dt <- renderDT({
    table <- session_frame()
    if (nrow(table) == 0) return(lift_empty_table("No hay filas para esa sesión"))
    datatable(table %>% transmute(Jugador = player, Puesto = position, Tarea = task, Valor = round(value, 2), Duración = round(duration_min, 1)), rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  })

  output$session_plot <- renderPlotly({
    table <- session_frame()
    if (nrow(table) == 0) return(lift_empty_plot("Sin datos para la sesión"))
    table$text <- paste0("Jugador: ", table$player, "<br>Valor: ", round(table$value, 2), "<br>Duración: ", round(table$duration_min, 1), " min")
    p <- ggplot(table, aes(stats::reorder(player, value), value, text = text)) + geom_col(fill = lift_palette$red, width = .68) + coord_flip() + labs(x = NULL, y = input$metric_session) + lift_theme_gg()
    lift_plotly(p, "text")
  })

  readiness_data <- reactive({
    req(context_data(), input$metric_readiness)
    context_data()
  })

  output$readiness_z_plot <- renderPlotly({
    req(readiness_data(), input$metric_readiness)
    snapshot <- gps_readiness_snapshot(readiness_data(), input$metric_readiness, input$z_window)
    snapshot <- snapshot[is.finite(snapshot$z), , drop = FALSE]
    if (nrow(snapshot) == 0) return(lift_empty_plot("Historia insuficiente", "Necesitás al menos una ventana previa completa por jugador."))
    snapshot$text <- paste0("Jugador: ", snapshot$player, "<br>Fecha: ", snapshot$date, "<br>Z-score: ", round(snapshot$z, 2), "<br>Banda: ", snapshot$band)
    p <- ggplot(snapshot, aes(stats::reorder(player, z), z, fill = band, text = text)) +
      geom_col(width = .65) +
      geom_hline(yintercept = -1.5, linetype = "dotted", colour = lift_palette$gray_dark) +
      geom_hline(yintercept = 0, linetype = "solid", colour = lift_palette$gray) +
      geom_hline(yintercept = 1.5, linetype = "dotted", colour = lift_palette$red) +
      coord_flip() + scale_fill_manual(values = c(high = lift_palette$red, low = lift_palette$gray, within = lift_palette$white, `not enough history` = lift_palette$gray_dark), drop = FALSE) + labs(x = NULL, y = "Z-score") + lift_theme_gg() + theme(legend.position = "none")
    lift_plotly(p, "text")
  })

  output$readiness_acwr_plot <- renderPlotly({
    req(readiness_data(), input$metric_readiness)
    snapshot <- gps_latest_acwr(readiness_data(), input$metric_readiness, input$acwr_acute, input$acwr_chronic)
    snapshot <- snapshot[is.finite(snapshot$acwr), , drop = FALSE]
    if (nrow(snapshot) == 0) return(lift_empty_plot("ACWR no disponible", "Revisá las ventanas y la cobertura temporal."))
    snapshot$text <- paste0("Jugador: ", snapshot$player, "<br>Fecha: ", snapshot$date, "<br>ACWR: ", round(snapshot$acwr, 2), "<br>Banda: ", snapshot$band)
    p <- ggplot(snapshot, aes(stats::reorder(player, acwr), acwr, fill = band, text = text)) + geom_col(width = .65) + geom_hline(yintercept = c(.8, 1.5), linetype = "dashed", colour = lift_palette$gray) + coord_flip() + scale_fill_manual(values = c(high = lift_palette$red, low = lift_palette$gray, within = lift_palette$white, `not available` = lift_palette$gray_dark), drop = FALSE) + labs(x = NULL, y = "ACWR") + lift_theme_gg() + theme(legend.position = "none")
    lift_plotly(p, "text")
  })

  output$readiness_table <- renderUI(lift_table_ui("readiness_table_dt"))
  output$readiness_table_dt <- renderDT({
    req(readiness_data(), input$metric_readiness)
    z <- gps_readiness_snapshot(readiness_data(), input$metric_readiness, input$z_window)
    acwr <- gps_latest_acwr(readiness_data(), input$metric_readiness, input$acwr_acute, input$acwr_chronic) %>% select(player, acwr, band)
    table <- full_join(z %>% select(player, date, value, z, band), acwr, by = "player", suffix = c("_z", "_acwr"))
    if (nrow(table) == 0) return(lift_empty_table("Sin suficiente historia"))
    datatable(table %>% transmute(Jugador = player, Fecha = date, Valor = round(value, 2), `Z-score` = round(z, 2), `Banda Z` = band_z, ACWR = round(acwr, 2), `Banda ACWR` = band_acwr), rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  })

  microcycle_data <- reactive({
    req(context_data(), input$metric_micro, input$micro_anchor)
    gps_microcycle_table(context_data(), input$metric_micro, input$micro_anchor, input$micro_training_days, input$micro_match_window)
  })

  output$microcycle_plot <- renderPlotly({
    table <- microcycle_data()
    table <- table[is.finite(table$ratio), , drop = FALSE]
    if (nrow(table) == 0) return(lift_empty_plot("Ratio no disponible", "Necesitás partido y entrenamiento acumulado para el mismo jugador."))
    table$text <- paste0("Jugador: ", table$player, "<br>Match promedio: ", round(table$match_value, 2), "<br>Training acumulado: ", round(table$training_value, 2), "<br>Ratio: ", round(table$ratio, 2))
    p <- ggplot(table, aes(stats::reorder(player, ratio), ratio, fill = ratio > 1.5, text = text)) + geom_col(width = .65, show.legend = FALSE) + geom_hline(yintercept = c(.8, 1.5), linetype = "dashed", colour = lift_palette$gray) + coord_flip() + scale_fill_manual(values = c(`FALSE` = lift_palette$white, `TRUE` = lift_palette$red)) + labs(x = NULL, y = "Match / training") + lift_theme_gg()
    lift_plotly(p, "text")
  })

  output$microcycle_table <- renderUI(lift_table_ui("microcycle_table_dt"))
  output$microcycle_table_dt <- renderDT({
    table <- microcycle_data()
    if (nrow(table) == 0) return(lift_empty_table("Sin match y training comparables"))
    datatable(table %>% transmute(Jugador = player, `Match promedio` = round(match_value, 2), `Training acumulado` = round(training_value, 2), Ratio = round(ratio, 2)), rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  })

  profile_data <- reactive({
    req(context_data(), input$profile_x, input$profile_y, input$profile_date)
    gps_quadrant_table(context_data(), input$profile_x, input$profile_y, input$profile_date)
  })

  output$quadrant_plot <- renderPlotly({
    table <- profile_data()
    if (nrow(table) == 0) return(lift_empty_plot("Perfil no disponible", "Elegí una sesión con ambas métricas visibles."))
    table$text <- paste0("Jugador: ", table$player, "<br>X: ", round(table$x, 2), "<br>Y: ", round(table$y, 2), "<br>Perfil: ", table$quadrant)
    colours <- c(`high / high` = lift_palette$red, `low / low` = lift_palette$gray, `high / low` = lift_palette$white, `low / high` = lift_palette$red_soft)
    p <- ggplot(table, aes(x, y, colour = quadrant, text = text)) +
      geom_vline(xintercept = unique(table$x_median), colour = lift_palette$gray_dark, linetype = "dashed") +
      geom_hline(yintercept = unique(table$y_median), colour = lift_palette$gray_dark, linetype = "dashed") +
      geom_point(size = 3.2, alpha = .9) + scale_colour_manual(values = colours) + labs(x = input$profile_x, y = input$profile_y, colour = "Perfil") + lift_theme_gg()
    lift_plotly(p, "text", legend = TRUE)
  })

  output$quadrant_table <- renderUI(lift_table_ui("quadrant_table_dt"))
  output$quadrant_table_dt <- renderDT({
    table <- profile_data()
    if (nrow(table) == 0) return(lift_empty_table("Sin jugadores comparables"))
    datatable(table %>% transmute(Jugador = player, `Métrica X` = round(x, 2), `Métrica Y` = round(y, 2), Perfil = quadrant), rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  })

  output$data_table <- renderDT({
    req(base_data())
    visible <- lift_visible_data(base_data())
    if (nrow(visible) == 0) return(lift_empty_table())
    datatable(visible, rownames = FALSE, extensions = "Buttons", options = list(pageLength = 15, scrollX = TRUE, dom = "Bfrtip", buttons = c("copy", "csv", "excel")))
  }, server = TRUE)

  output$download_filtered <- downloadHandler(
    filename = function() paste0("gps_lift_visible_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(path) {
      data <- if (is.null(base_data())) data.frame() else lift_visible_data(base_data())
      readr::write_csv(data, path, na = "")
    }
  )
}
