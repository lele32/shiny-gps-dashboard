suppressWarnings(suppressPackageStartupMessages(library(dplyr)))

source(testthat::test_path("../../R/gps_helpers.R"), local = TRUE)
source(testthat::test_path("../../R/gps_semantics.R"), local = TRUE)

gps_fixture <- function() {
  data.frame(
    Player = c("A", "B", "A", "B", "A", "B"),
    Date = c("2026-01-01", "2026-01-01", "2026-01-02", "2026-01-02", "2026-01-03", "2026-01-03"),
    `Session Type` = c("Training", "Match", "Training", "Match", "Training", "Match"),
    Task = c("Field", "Match", "Field", "Match", "Gym", "Match"),
    `Total Distance (m)` = c(1000, 2000, 1100, 2100, 500, 2200),
    `Speed (km/h)` = c(5, 6, 5.5, 6.5, 4, 7),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

test_that("context derivation always preserves row cardinality", {
  data <- gps_fixture()
  result <- gps_derive_context(data, list(
    player = "Player", date = "Date", matchday = "Session Type", task = "Task"
  ))

  expect_equal(nrow(result), nrow(data))
  expect_length(result$.gps_duration_min, nrow(data))
  expect_equal(result$.gps_session_type[c(1, 2)], c("training", "match"))
})

test_that("mapped session type takes precedence over match-day labels", {
  data <- data.frame(
    Player = c("A", "B"),
    Date = c("2026-01-01", "2026-01-01"),
    `Match Day` = c("No MD", "No MD"),
    Session = c("Match", "Physical"),
    Distance = c(1000, 1200),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result <- gps_derive_context(data, list(
    player = "Player", date = "Date", matchday = "Match Day", session_type = "Session"
  ))

  expect_equal(result$.gps_session_type, c("match", "training"))
})

test_that("multi-select session filters keep every selected session", {
  data <- gps_derive_context(gps_fixture(), list(
    player = "Player", date = "Date", matchday = "Session Type", task = "Task"
  ))

  result <- gps_apply_filters(data, session_type = c("match", "training"))

  expect_equal(nrow(result), nrow(data))
  expect_setequal(unique(result$.gps_session_type), c("match", "training"))
})

test_that("daily metric aggregation respects units", {
  data <- gps_derive_context(gps_fixture(), list(
    player = "Player", date = "Date", matchday = "Session Type", task = "Task"
  ))

  distance <- gps_daily_metric(data, "Total Distance (m)")
  speed <- gps_daily_metric(data, "Speed (km/h)")
  matchday <- gps_matchday_frame(data, "Total Distance (m)")

  expect_equal(distance$value[distance$player == "A" & distance$date == as.Date("2026-01-01")], 1000)
  expect_equal(speed$value[speed$player == "A" & speed$date == as.Date("2026-01-01")], 5)
  expect_equal(nrow(matchday), 6)
})

test_that("readiness tables keep prior history and expose interpretable bands", {
  data <- gps_derive_context(gps_fixture(), list(
    player = "Player", date = "Date", matchday = "Session Type", task = "Task"
  ))

  z <- gps_zscore_table(data, "Total Distance (m)", window = 2)
  acwr <- gps_acwr_table(data, "Total Distance (m)", acute_days = 3, chronic_days = 5)

  expect_true(all(c("baseline_mean", "baseline_sd", "z", "band") %in% names(z)))
  expect_true(all(z$band[is.na(z$z)] == "not enough history"))
  expect_true(all(c("acute", "chronic", "acwr", "band") %in% names(acwr)))
})

test_that("EWMA decays according to calendar gaps", {
  dense <- gps_ewma(c(100, 0), half_life = 2, dates = as.Date(c("2026-01-01", "2026-01-02")))
  gap <- gps_ewma(c(100, 0), half_life = 2, dates = as.Date(c("2026-01-01", "2026-01-05")))

  expect_lt(gap[[2]], dense[[2]])
})

test_that("microcycle and quadrant views return player-level decision tables", {
  data <- gps_derive_context(gps_fixture(), list(
    player = "Player", date = "Date", matchday = "Session Type", task = "Task"
  ))

  micro <- gps_microcycle_table(data, "Total Distance (m)", as.Date("2026-01-03"), training_days = 3, match_window = 2)
  quadrant <- gps_quadrant_table(data, "Total Distance (m)", "Speed (km/h)", as.Date("2026-01-03"))

  expect_true(all(c("player", "match_value", "training_value", "ratio") %in% names(micro)))
  expect_equal(nrow(quadrant), 2)
  expect_true(all(quadrant$quadrant %in% c("high / high", "low / low", "high / low", "low / high")))
})
