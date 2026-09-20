gps_normalize_label <- function(x) {
  x <- iconv(as.character(x), to = "ASCII//TRANSLIT")
  gsub("[^a-z0-9]+", "", tolower(x))
}

gps_detect_provider <- function(source_name = "", data = NULL) {
  labels <- c(source_name, if (is.data.frame(data)) names(data) else character(0))
  fingerprint <- tolower(paste(labels, collapse = " "))
  if (grepl("catapult|athleteload|openfield", fingerprint)) return("catapult")
  if (grepl("wimu|pro2", fingerprint)) return("wimu")
  if (grepl("stats[ -]?sports|statsports", fingerprint)) return("stats_sports")
  "unknown"
}

gps_metric_unit <- function(label) {
  value <- tolower(trimws(as.character(label)))
  if (grepl("km\\s*/\\s*h|kmh", value)) return("km/h")
  if (grepl("\\(\\s*m\\s*\\)|\\[\\s*m\\s*\\]|(^|[_ -])meters?($|[_ -])", value)) return("m")
  if (grepl("\\(\\s*s(ec(onds?)?)?\\s*\\)|seconds?", value)) return("s")
  if (grepl("minutes?|mins?", value)) return("min")
  if (grepl("reps?|count|number", value)) return("count")
  "unknown"
}

gps_role_aliases <- list(
  player = c("player", "player name", "username", "athlete", "athlete name", "subject", "jugador"),
  position = c("position", "pos", "role", "rol", "puesto"),
  matchday = c("match day", "matchday", "match.day", "md", "match type", "session type", "dia partido", "dia"),
  task = c("task", "activity", "activity name", "drill", "selection", "tarea", "ejercicio"),
  session_type = c("session", "session type", "type session", "session category", "activity type", "tipo sesion", "tipo de sesion"),
  date = c("date", "fecha", "session date", "recorded date", "day", "dia sesion"),
  duration = c("duration", "duration min", "duration minutes", "duration_min", "duracion", "duracion min"),
  start = c("start", "start time", "inicio", "hora inicio", "start.hour"),
  end = c("end", "end time", "fin", "hora fin", "end time", "end_time", "final hour")
)

gps_guess_column <- function(cols, role) {
  aliases <- gps_role_aliases[[role]]
  if (is.null(aliases)) {
    aliases <- role
  }
  normalized_cols <- gps_normalize_label(cols)
  normalized_aliases <- gps_normalize_label(aliases)
  exact <- match(normalized_aliases, normalized_cols, nomatch = 0L)
  if (any(exact > 0L)) {
    return(cols[exact[which(exact > 0L)[1L]]])
  }
  NULL
}

gps_numeric <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  values <- trimws(as.character(x))
  values[values == ""] <- NA_character_
  both_separators <- !is.na(values) & grepl(",", values, fixed = TRUE) & grepl("\\.", values)
  comma_decimal <- !is.na(values) & grepl(",", values, fixed = TRUE) &
    (!grepl("\\.", values) | (nchar(values) - regexpr(",", values, fixed = TRUE) <
      nchar(values) - regexpr("\\.", values)))
  values[both_separators & comma_decimal] <- gsub("\\.", "", values[both_separators & comma_decimal])
  values[comma_decimal] <- sub(",", ".", values[comma_decimal], fixed = TRUE)
  values[!comma_decimal & grepl(",", values, fixed = TRUE)] <-
    gsub(",", "", values[!comma_decimal & grepl(",", values, fixed = TRUE)], fixed = TRUE)
  values <- gsub("[^0-9eE+.-]", "", values)
  suppressWarnings(as.numeric(values))
}

gps_numericish <- function(x, minimum_valid_fraction = 0.8) {
  if (is.numeric(x)) {
    return(TRUE)
  }
  values <- gps_numeric(x)
  non_empty <- !is.na(x) & trimws(as.character(x)) != ""
  if (!any(non_empty)) {
    return(FALSE)
  }
  mean(!is.na(values[non_empty])) >= minimum_valid_fraction
}

gps_metric_candidates <- function(data) {
  excluded <- c("player", "athlete", "username", "subject", "jugador", "position", "pos", "role",
                "rol", "puesto", "date", "fecha", "day", "task", "activity", "drill", "selection",
                "tarea", "matchday", "matchday", "md", "start", "end", "time", "hora",
                "source", "provider", "row")
  normalized <- gps_normalize_label(names(data))
  context_columns <- unique(stats::na.omit(vapply(
    c("player", "position", "matchday", "task", "session_type", "date", "start", "end"),
    function(role) {
      value <- gps_guess_column(names(data), role)
      if (is.null(value) || length(value) == 0L) NA_character_ else value[[1L]]
    },
    character(1)
  )))
  metadata_column <- grepl("^(week|num|repetition|signal)|^(a|b|r2)$", normalized)
  candidates <- vapply(seq_along(data), function(i) {
    gps_numericish(data[[i]]) &&
      !(names(data)[[i]] %in% context_columns) &&
      !metadata_column[[i]] &&
      !any(vapply(excluded, function(term) grepl(term, normalized[i], fixed = TRUE), logical(1)))
  }, logical(1))
  result <- names(data)[candidates]
  if (length(result) <= 1L) return(result)
  priority <- vapply(result, function(label) {
    normalized_label <- gps_normalize_label(label)
    score <- if (grepl("^(total)?distance", normalized_label)) {
      130
    } else if (grepl("explosive.*(distance|dist)|((distance|dist).*explosive)", normalized_label)) {
      127
    } else if (grepl("hibd", normalized_label)) {
      125
    } else if (grepl("hmld", normalized_label)) {
      118
    } else if (grepl("playerload", normalized_label)) {
      115
    } else if (grepl("sprint|hsr", normalized_label)) {
      100
    } else if (grepl("speed|acceleration|deceleration|accel|decel", normalized_label)) {
      90
    } else if (grepl("heart|hr|power|energy|metabolic", normalized_label)) {
      80
    } else {
      20
    }
    score + ifelse(grepl("duration|count|number", normalized_label), 5, 0)
  }, numeric(1))
  result[order(-priority, match(result, names(data)))]
}

gps_detect_delimiter <- function(lines) {
  candidates <- c(",", ";", "\t")
  counts <- vapply(candidates, function(delim) {
    sum(vapply(lines, function(line) grepl(delim, line, fixed = TRUE), logical(1)))
  }, integer(1))
  candidates[which.max(counts)]
}

gps_detect_header_skip <- function(lines, delimiter) {
  header_terms <- c("player", "athlete", "username", "subject", "jugador", "date", "fecha", "session", "activity")
  scores <- vapply(lines, function(line) {
    fields <- strsplit(line, delimiter, fixed = TRUE)[[1L]]
    normalized <- gps_normalize_label(fields)
    has_header_term <- any(vapply(header_terms, function(term) {
      any(grepl(term, normalized, fixed = TRUE))
    }, logical(1)))
    if (has_header_term && length(fields) >= 2L) length(fields) else 0L
  }, integer(1))
  index <- which(scores > 0L)
  if (length(index) == 0L) 0L else index[1L] - 1L
}

gps_read_source_file <- function(path, extension) {
  extension <- tolower(as.character(extension))
  if (grepl("\\.", extension)) {
    extension <- tools::file_ext(extension)
  }
  extension <- sub("^\\.", "", extension)
  result <- switch(
    extension,
    csv = {
      lines <- readr::read_lines(path, n_max = 100L, progress = FALSE)
      delimiter <- gps_detect_delimiter(lines)
      skip <- gps_detect_header_skip(lines, delimiter)
      readr::read_delim(
        path,
        delim = delimiter,
        skip = skip,
        trim_ws = TRUE,
        name_repair = "minimal",
        col_types = readr::cols(.default = readr::col_character()),
        locale = readr::locale(encoding = "UTF-8"),
        show_col_types = FALSE,
        progress = FALSE
      )
    },
    xls = readxl::read_excel(path),
    xlsx = readxl::read_excel(path),
    json = jsonlite::fromJSON(path, flatten = TRUE),
    stop("Unsupported file type: ", extension, call. = FALSE)
  )
  if (!is.data.frame(result)) {
    result <- as.data.frame(result, stringsAsFactors = FALSE)
  }
  names(result) <- make.unique(trimws(names(result)))
  result
}

gps_parse_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  parsed <- suppressWarnings(lubridate::parse_date_time(
    as.character(x),
    orders = c("ymd", "dmy", "mdy", "Y-m-d H:M:S", "d/m/Y H:M:S", "m/d/Y H:M:S"),
    quiet = TRUE
  ))
  as.Date(parsed)
}

gps_parse_clock <- function(x) {
  parsed <- suppressWarnings(lubridate::parse_date_time(
    as.character(x),
    orders = c("H:M:S", "H:M", "HMS", "HM"),
    quiet = TRUE,
    tz = "UTC"
  ))
  as.POSIXct(parsed, tz = "UTC")
}

gps_duration_minutes <- function(duration = NULL, start = NULL, end = NULL) {
  length_hint <- max(length(duration), length(start), length(end), 0L)
  if (!is.null(duration)) {
    direct <- gps_numeric(duration)
  } else {
    direct <- rep(NA_real_, length_hint)
  }
  if (is.null(start) || is.null(end)) {
    return(direct)
  }
  start_time <- gps_parse_clock(start)
  end_time <- gps_parse_clock(end)
  derived <- as.numeric(difftime(end_time, start_time, units = "mins"))
  overnight <- is.finite(derived) & derived < 0
  derived[overnight] <- derived[overnight] + 24 * 60
  ifelse(!is.na(direct), direct, derived)
}

gps_classify_session <- function(x) {
  normalized <- gps_normalize_label(x)
  result <- rep("unknown", length(normalized))
  result[grepl("^(md|md[0-9]+|[0-9]+md|match|game|partido|juego|officialmatch|officialgame|friendlygame|matchday)$", normalized)] <- "match"
  result[grepl("^(training|practice|entreno|entrenamiento|session|sesion|gym|recovery|physical|optimizerdisplacement|nomd)$", normalized)] <- "training"
  result
}

gps_deduplicate_rows <- function(data) {
  if (!is.data.frame(data) || nrow(data) < 2L) {
    return(data)
  }
  data[!duplicated(data), , drop = FALSE]
}

gps_rolling_z <- function(values, window = 5L) {
  values <- gps_numeric(values)
  result <- rep(NA_real_, length(values))
  if (length(values) <= window) {
    return(result)
  }
  for (i in seq.int(window + 1L, length(values))) {
    baseline <- values[(i - window):(i - 1L)]
    if (sum(is.finite(baseline)) < window) {
      next
    }
    spread <- stats::sd(baseline)
    if (is.finite(spread) && spread > 0 && is.finite(values[i])) {
      result[i] <- (values[i] - mean(baseline)) / spread
    }
  }
  result
}

gps_latest_rolling_z <- function(data, player_col, date_col, metric, window = 5L) {
  if (!is.data.frame(data) || nrow(data) == 0L ||
      !all(c(player_col, date_col, metric) %in% names(data))) {
    return(data.frame(
      Jugador = character(0),
      ultima_fecha = as.Date(character(0)),
      Valor = numeric(0),
      z_ultimo = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  frame <- data.frame(
    Jugador = as.character(data[[player_col]]),
    Fecha = gps_parse_date(data[[date_col]]),
    Valor = gps_numeric(data[[metric]]),
    stringsAsFactors = FALSE
  )
  frame <- frame[!is.na(frame$Jugador) & nzchar(frame$Jugador) &
                   !is.na(frame$Fecha) & is.finite(frame$Valor), , drop = FALSE]
  if (nrow(frame) == 0L) {
    return(data.frame(
      Jugador = character(0),
      ultima_fecha = as.Date(character(0)),
      Valor = numeric(0),
      z_ultimo = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  groups <- split(seq_len(nrow(frame)), frame$Jugador, drop = TRUE)
  result <- lapply(groups, function(index) {
    index <- index[order(frame$Fecha[index], index)]
    z_values <- gps_rolling_z(frame$Valor[index], window = window)
    last_index <- length(index)
    data.frame(
      Jugador = frame$Jugador[index[last_index]],
      ultima_fecha = frame$Fecha[index[last_index]],
      Valor = frame$Valor[index[last_index]],
      z_ultimo = z_values[last_index],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, result)
}

gps_match_training_ratio <- function(match, training) {
  match <- gps_numeric(match)
  training <- gps_numeric(training)
  ifelse(is.finite(training) & training > 0, match / training, NA_real_)
}
