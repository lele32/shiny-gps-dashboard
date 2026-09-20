# Canonical GPS data layer -------------------------------------------------
#
# The Shiny layer should ask this module for a frame, a metric definition or a
# decision table. It should not know how Catapult, WIMU or Stats Sports encode
# dates, sessions, durations or numeric text.

gps_mapping_value <- function(mapping, key) {
  value <- mapping[[key]]
  if (is.null(value) || length(value) == 0 || is.na(value[[1]]) ||
      !nzchar(as.character(value[[1]])) || identical(as.character(value[[1]]), "None")) {
    return(NULL)
  }
  as.character(value[[1]])
}

gps_has_column <- function(data, column) {
  is.data.frame(data) && !is.null(column) && length(column) == 1L &&
    !is.na(column) && column %in% names(data)
}

gps_first_value <- function(x) {
  x <- x[!is.na(x) & nzchar(as.character(x))]
  if (length(x) == 0) NA_character_ else as.character(x[[1]])
}

gps_derive_context <- function(data, mapping) {
  if (!is.data.frame(data)) return(data.frame())
  out <- data

  player_col <- gps_mapping_value(mapping, "player")
  position_col <- gps_mapping_value(mapping, "position")
  matchday_col <- gps_mapping_value(mapping, "matchday")
  task_col <- gps_mapping_value(mapping, "task")
  session_type_col <- gps_mapping_value(mapping, "session_type")
  date_col <- gps_mapping_value(mapping, "date")
  duration_col <- gps_mapping_value(mapping, "duration")
  start_col <- gps_mapping_value(mapping, "start")
  end_col <- gps_mapping_value(mapping, "end")

  out$.gps_player <- if (gps_has_column(out, player_col)) as.character(out[[player_col]]) else NA_character_
  out$.gps_position <- if (gps_has_column(out, position_col)) as.character(out[[position_col]]) else NA_character_
  out$.gps_matchday <- if (gps_has_column(out, matchday_col)) as.character(out[[matchday_col]]) else NA_character_
  out$.gps_task <- if (gps_has_column(out, task_col)) as.character(out[[task_col]]) else NA_character_
  out$.gps_date <- if (gps_has_column(out, date_col)) gps_parse_date(out[[date_col]]) else as.Date(NA)
  matchday_session_type <- gps_classify_session(out$.gps_matchday)
  mapped_session_type <- if (gps_has_column(out, session_type_col)) {
    gps_classify_session(out[[session_type_col]])
  } else {
    rep("unknown", nrow(out))
  }
  out$.gps_session_type <- mapped_session_type
  unknown_session <- is.na(out$.gps_session_type) | out$.gps_session_type == "unknown"
  out$.gps_session_type[unknown_session] <- matchday_session_type[unknown_session]
  has_duration_context <- gps_has_column(out, duration_col) ||
    gps_has_column(out, start_col) || gps_has_column(out, end_col)
  out$.gps_duration_min <- if (has_duration_context) {
    gps_duration_minutes(
      if (gps_has_column(out, duration_col)) out[[duration_col]] else NULL,
      if (gps_has_column(out, start_col)) out[[start_col]] else NULL,
      if (gps_has_column(out, end_col)) out[[end_col]] else NULL
    )
  } else {
    rep(NA_real_, nrow(out))
  }
  out$.gps_source <- if (".gps_source_name" %in% names(out)) as.character(out$.gps_source_name) else NA_character_
  out$.gps_provider <- if (".gps_provider" %in% names(out)) as.character(out$.gps_provider) else "unknown"
  out
}

gps_metric_aggregation <- function(metric) {
  unit <- gps_metric_unit(metric)
  if (unit %in% c("m", "s", "min", "count")) "sum" else "mean"
}

gps_metric_catalog <- function(data) {
  metrics <- gps_metric_candidates(data)
  if (length(metrics) == 0) {
    return(data.frame(
      metric = character(0), unit = character(0), aggregation = character(0),
      valid_n = integer(0), coverage = numeric(0), stringsAsFactors = FALSE
    ))
  }
  result <- lapply(metrics, function(metric) {
    values <- gps_numeric(data[[metric]])
    valid <- is.finite(values)
    data.frame(
      metric = metric,
      unit = gps_metric_unit(metric),
      aggregation = gps_metric_aggregation(metric),
      valid_n = sum(valid),
      coverage = if (length(values) == 0) 0 else mean(valid),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, result)
}

gps_apply_filters <- function(data, player = NULL, position = NULL, matchday = NULL,
                              task = NULL, date_range = NULL, duration_range = NULL,
                              session_type = NULL) {
  if (!is.data.frame(data) || nrow(data) == 0) return(data)
  keep <- rep(TRUE, nrow(data))
  keep_selection <- function(values, selected) {
    is.null(selected) || length(selected) == 0 || all(is.na(selected)) ||
      as.character(values) %in% as.character(selected)
  }
  keep <- keep & keep_selection(data$.gps_player, player)
  keep <- keep & keep_selection(data$.gps_position, position)
  keep <- keep & keep_selection(data$.gps_matchday, matchday)
  keep <- keep & keep_selection(data$.gps_task, task)
  keep <- keep & keep_selection(data$.gps_session_type, session_type)

  if (!is.null(date_range) && length(date_range) == 2L) {
    keep <- keep & !is.na(data$.gps_date) & data$.gps_date >= as.Date(date_range[[1]]) &
      data$.gps_date <= as.Date(date_range[[2]])
  }
  if (!is.null(duration_range) && length(duration_range) == 2L) {
    keep <- keep & is.finite(data$.gps_duration_min) &
      data$.gps_duration_min >= duration_range[[1]] & data$.gps_duration_min <= duration_range[[2]]
  }
  data[keep %in% TRUE, , drop = FALSE]
}

gps_metric_frame <- function(data, metric, aggregation = NULL) {
  if (!is.data.frame(data) || !gps_has_column(data, metric)) return(data.frame())
  values <- gps_numeric(data[[metric]])
  frame <- data.frame(
    player = as.character(data$.gps_player),
    position = as.character(data$.gps_position),
    matchday = as.character(data$.gps_matchday),
    session_type = as.character(data$.gps_session_type),
    task = as.character(data$.gps_task),
    date = as.Date(data$.gps_date),
    duration_min = gps_numeric(data$.gps_duration_min),
    value = values,
    stringsAsFactors = FALSE
  )
  frame <- frame[!is.na(frame$player) & nzchar(frame$player) & !is.na(frame$date) & is.finite(frame$value), , drop = FALSE]
  if (nrow(frame) == 0) return(frame)
  method <- aggregation %||% gps_metric_aggregation(metric)
  frame$method <- method
  frame
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x[[1]])) y else x

gps_daily_metric <- function(data, metric, aggregation = NULL) {
  frame <- gps_metric_frame(data, metric, aggregation)
  if (nrow(frame) == 0) return(frame)
  method <- unique(frame$method)[[1]]
  frame %>%
    group_by(player, date) %>%
    summarise(
      value = if (.env$method == "sum") sum(value, na.rm = TRUE) else mean(value, na.rm = TRUE),
      position = gps_first_value(position),
      matchday = gps_first_value(matchday),
      session_type = gps_first_value(session_type),
      task = gps_first_value(task),
      duration_min = if (any(is.finite(duration_min))) sum(duration_min, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>%
    arrange(player, date)
}

gps_latest_date <- function(data) {
  dates <- if (is.data.frame(data) && ".gps_date" %in% names(data)) data$.gps_date else as.Date(NA)
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) as.Date(NA) else max(dates)
}

gps_overview_summary <- function(data, metric) {
  daily <- gps_daily_metric(data, metric)
  if (nrow(daily) == 0) {
    return(data.frame(metric = metric, sessions = 0L, players = 0L, latest = as.Date(NA),
                      mean = NA_real_, latest_mean = NA_real_, stringsAsFactors = FALSE))
  }
  latest <- max(daily$date, na.rm = TRUE)
  data.frame(
    metric = metric,
    sessions = length(unique(daily$date)),
    players = length(unique(daily$player)),
    latest = latest,
    mean = mean(daily$value, na.rm = TRUE),
    latest_mean = mean(daily$value[daily$date == latest], na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

gps_zscore_table <- function(data, metric, window = 5L) {
  daily <- gps_daily_metric(data, metric)
  if (nrow(daily) == 0) return(daily)
  window <- max(2L, as.integer(window[[1]]))
  daily %>%
    group_by(player) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(
      baseline_mean = dplyr::lag(slider::slide_dbl(value, mean, .before = window - 1L, .complete = TRUE)),
      baseline_sd = dplyr::lag(slider::slide_dbl(value, sd, .before = window - 1L, .complete = TRUE)),
      z = (value - baseline_mean) / baseline_sd,
      band = case_when(
        is.finite(z) & z >= 1.5 ~ "high",
        is.finite(z) & z <= -1.5 ~ "low",
        is.finite(z) ~ "within",
        TRUE ~ "not enough history"
      ),
      .groups = "drop"
    ) %>%
    ungroup()
}

gps_readiness_snapshot <- function(data, metric, window = 5L) {
  table <- gps_zscore_table(data, metric, window)
  if (nrow(table) == 0) return(table)
  table %>%
    group_by(player) %>%
    slice_max(date, n = 1L, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(desc(z))
}

gps_matchday_frame <- function(data, metric) {
  frame <- gps_metric_frame(data, metric)
  if (nrow(frame) == 0) return(frame)
  frame <- frame[!is.na(frame$matchday) & nzchar(frame$matchday), , drop = FALSE]
  if (nrow(frame) == 0) return(frame)
  method <- unique(frame$method)[[1]]
  frame %>%
    group_by(player, date, matchday) %>%
    summarise(
      value = if (.env$method == "sum") sum(value, na.rm = TRUE) else mean(value, na.rm = TRUE),
      .groups = "drop"
    )
}

gps_ewma <- function(values, half_life, dates = NULL) {
  values <- gps_numeric(values)
  if (length(values) == 0) return(numeric(0))
  dates <- if (is.null(dates)) as.Date(rep(NA_character_, length(values))) else gps_parse_date(dates)
  lambda <- 1 - exp(log(0.5) / max(1, half_life))
  result <- rep(NA_real_, length(values))
  for (i in seq_along(values)) {
    if (!is.finite(values[[i]])) next
    if (i == 1L || !is.finite(result[[i - 1L]])) {
      result[[i]] <- values[[i]]
      next
    }
    gap <- if (!is.na(dates[[i]]) && !is.na(dates[[i - 1L]])) {
      max(1, as.numeric(difftime(dates[[i]], dates[[i - 1L]], units = "days")))
    } else {
      1
    }
    date_lambda <- 1 - exp(log(0.5) * gap / max(1, half_life))
    result[[i]] <- date_lambda * values[[i]] + (1 - date_lambda) * result[[i - 1L]]
  }
  result
}

gps_acwr_table <- function(data, metric, acute_days = 7L, chronic_days = 28L) {
  daily <- gps_daily_metric(data, metric)
  if (nrow(daily) == 0) return(daily)
  daily %>%
    group_by(player) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(
      acute = gps_ewma(value, acute_days, date),
      chronic = gps_ewma(value, chronic_days, date),
      acwr = ifelse(is.finite(chronic) & chronic > 0, acute / chronic, NA_real_),
      band = case_when(
        is.finite(acwr) & acwr > 1.5 ~ "high",
        is.finite(acwr) & acwr < 0.8 ~ "low",
        is.finite(acwr) ~ "within",
        TRUE ~ "not available"
      ),
      .groups = "drop"
    ) %>%
    ungroup()
}

gps_latest_acwr <- function(data, metric, acute_days = 7L, chronic_days = 28L) {
  table <- gps_acwr_table(data, metric, acute_days, chronic_days)
  if (nrow(table) == 0) return(table)
  table %>% group_by(player) %>% slice_max(date, n = 1L, with_ties = FALSE) %>% ungroup()
}

gps_session_table <- function(data, metric, session_date) {
  if (is.null(session_date) || length(session_date) == 0 || is.na(session_date[[1]])) return(data.frame())
  frame <- gps_metric_frame(data, metric)
  if (nrow(frame) == 0) return(frame)
  method <- unique(frame$method)[[1]]
  frame %>%
    filter(date == as.Date(session_date[[1]])) %>%
    group_by(player, position, task, session_type) %>%
    summarise(
      value = if (.env$method == "sum") sum(value, na.rm = TRUE) else mean(value, na.rm = TRUE),
      duration_min = if (any(is.finite(duration_min))) sum(duration_min, na.rm = TRUE) else NA_real_, .groups = "drop"
    ) %>%
    arrange(desc(value))
}

gps_microcycle_table <- function(data, metric, anchor_date, training_days = 7L, match_window = 3L) {
  daily <- gps_daily_metric(data, metric)
  if (nrow(daily) == 0 || is.null(anchor_date) || is.na(anchor_date[[1]])) return(data.frame())
  anchor_date <- as.Date(anchor_date[[1]])
  matches <- daily %>% filter(session_type == "match", date <= anchor_date) %>%
    group_by(player) %>% slice_tail(n = match_window) %>%
    summarise(match_value = mean(value, na.rm = TRUE), .groups = "drop")
  training <- daily %>% filter(session_type == "training", date < anchor_date,
                               date >= anchor_date - training_days) %>%
    group_by(player) %>% summarise(training_value = sum(value, na.rm = TRUE), .groups = "drop")
  full_join(matches, training, by = "player") %>%
    mutate(ratio = gps_match_training_ratio(match_value, training_value), anchor_date = anchor_date)
}

gps_quadrant_table <- function(data, metric_x, metric_y, session_date) {
  if (is.null(session_date) || length(session_date) == 0 || is.na(session_date[[1]])) return(data.frame())
  x_frame <- gps_metric_frame(data, metric_x) %>% filter(date == as.Date(session_date[[1]]))
  x_method <- if (nrow(x_frame) == 0) "mean" else unique(x_frame$method)[[1]]
  x <- x_frame %>%
    group_by(player) %>% summarise(
      x = if (.env$x_method == "sum") sum(value, na.rm = TRUE) else mean(value, na.rm = TRUE),
      .groups = "drop"
    )
  y_frame <- gps_metric_frame(data, metric_y) %>% filter(date == as.Date(session_date[[1]]))
  y_method <- if (nrow(y_frame) == 0) "mean" else unique(y_frame$method)[[1]]
  y <- y_frame %>%
    group_by(player) %>% summarise(
      y = if (.env$y_method == "sum") sum(value, na.rm = TRUE) else mean(value, na.rm = TRUE),
      .groups = "drop"
    )
  result <- inner_join(x, y, by = "player")
  if (nrow(result) == 0) return(result)
  x_mid <- median(result$x, na.rm = TRUE)
  y_mid <- median(result$y, na.rm = TRUE)
  result %>% mutate(
    x_median = x_mid, y_median = y_mid,
    quadrant = case_when(
      x >= x_mid & y >= y_mid ~ "high / high",
      x < x_mid & y < y_mid ~ "low / low",
      x >= x_mid & y < y_mid ~ "high / low",
      TRUE ~ "low / high"
    )
  )
}
