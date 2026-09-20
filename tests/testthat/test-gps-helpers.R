source(testthat::test_path("../../R/gps_helpers.R"), local = TRUE)

test_that("exact duplicate rows are removed without collapsing same-day activities", {
  data <- data.frame(
    Player = c("A", "A", "A"),
    Date = c("2026-01-01", "2026-01-01", "2026-01-01"),
    Task = c("Training", "Gym", "Training"),
    Distance = c(1000, 500, 1000),
    stringsAsFactors = FALSE
  )

  result <- gps_deduplicate_rows(data)

  expect_equal(nrow(result), 2)
  expect_setequal(result$Task, c("Training", "Gym"))
})

test_that("session classification accepts provider match-day variants", {
  expect_equal(gps_classify_session(c("MD", "MD-1", "Match", "Training", "MD+2")),
               c("match", "match", "match", "training", "match"))
})

test_that("duration parsing handles decimal values and sessions crossing midnight", {
  expect_equal(gps_duration_minutes(c("45,5", NA), c(NA, "23:50"), c(NA, "00:20")),
               c(45.5, 30))
  expect_equal(gps_numeric(c("1.234,5", "1,234.5")), c(1234.5, 1234.5))
})

test_that("column guessing and metric discovery support numeric text metrics", {
  data <- data.frame(
    `Athlete Name` = c("A", "B"),
    `Session Date` = c("2026-01-01", "2026-01-02"),
    `Total Distance (m)` = c("1000,5", "1200,0"),
    Task = c("Training", "Match"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  expect_equal(gps_guess_column(names(data), "player"), "Athlete Name")
  expect_equal(gps_guess_column(names(data), "date"), "Session Date")
  expect_setequal(gps_metric_candidates(data), "Total Distance (m)")
})

test_that("metric discovery removes time and export metadata columns", {
  data <- data.frame(
    Player = c("A", "B"),
    Date = c("2026-01-01", "2026-01-01"),
    `Start hour` = c("20:00:00", "20:00:00"),
    `Final Hour` = c("21:00:00", "21:00:00"),
    `Week Team` = c(1, 1),
    `Distance(m)` = c(1000, 1200),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  expect_equal(gps_metric_candidates(data), "Distance(m)")
})

test_that("provider and unit metadata are detected from GPS exports", {
  data <- data.frame(`WIMU Player` = "A", check.names = FALSE)

  expect_equal(gps_detect_provider("wimu_session_export.csv", data), "wimu")
  expect_equal(gps_metric_unit("Total Distance (m)"), "m")
  expect_equal(gps_metric_unit("High Speed Running (km/h)"), "km/h")
})

test_that("rolling z-score uses only prior observations", {
  result <- gps_rolling_z(c(10, 12, 11, 20), window = 3)

  expect_true(all(is.na(result[1:3])))
  expect_true(is.finite(result[4]))
  expect_gt(result[4], 0)
})

test_that("latest rolling z-score is calculated per player", {
  data <- data.frame(
    Player = c("A", "A", "A", "A", "B", "B", "B", "B"),
    Date = rep(sprintf("2026-01-0%d", 1:4), 2),
    Load = c(10, 12, 11, 20, 20, 22, 21, 10),
    stringsAsFactors = FALSE
  )

  result <- gps_latest_rolling_z(data, "Player", "Date", "Load", window = 3)

  expect_equal(nrow(result), 2)
  expect_gt(result$z_ultimo[result$Jugador == "A"], 0)
  expect_lt(result$z_ultimo[result$Jugador == "B"], 0)
})

test_that("microcycle ratio is explicitly match divided by training", {
  expect_equal(gps_match_training_ratio(match = 100, training = 50), 2)
  expect_true(is.na(gps_match_training_ratio(match = 100, training = 0)))
})

test_that("CSV import handles metadata rows and semicolon delimiters", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    "Catapult export;generated 2026-01-01",
    "Player Name;Date;Total Distance (m)",
    "A;2026-01-01;1000,5"
  ), path)

  result <- gps_read_source_file(path, "csv")

  expect_equal(names(result), c("Player Name", "Date", "Total Distance (m)"))
  expect_equal(result[["Player Name"]], "A")
  expect_equal(gps_numeric(result[["Total Distance (m)"]]), 1000.5)
})
