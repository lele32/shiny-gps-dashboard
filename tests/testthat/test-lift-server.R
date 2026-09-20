suppressWarnings(suppressPackageStartupMessages({
  library(shiny)
  library(readr)
  library(readxl)
  library(jsonlite)
  library(DT)
  library(ggplot2)
  library(plotly)
  library(dplyr)
  library(lubridate)
  library(slider)
  library(bslib)
}))

source(testthat::test_path("../../R/gps_helpers.R"), local = TRUE)
source(testthat::test_path("../../R/gps_semantics.R"), local = TRUE)
source(testthat::test_path("../../R/lift_ui.R"), local = TRUE)
source(testthat::test_path("../../R/lift_server.R"), local = TRUE)

test_that("workspace exposes every sidebar view as a real tab", {
  rendered <- paste(as.character(htmltools::renderTags(ui)$html), collapse = "")
  nav_links <- regmatches(rendered, gregexpr('data-toggle="tab"[^>]*data-value="[^"]+"', rendered, perl = TRUE))[[1]]

  expect_length(nav_links, length(lift_nav_choices))
})

test_that("server imports a file and renders the overview contract", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    "Player,Date,Matchday,Task,Total Distance (m),Speed (km/h),Duration",
    "A,2026-01-01,Training,Field,1000,5,45",
    "B,2026-01-01,Match,Match,2000,6,90",
    "A,2026-01-02,Training,Field,1100,5.5,50",
    "B,2026-01-02,Match,Match,2100,6.5,90"
  ), path)

  testServer(gps_server, {
    session$setInputs(file = NULL)
    session$flushReact()
    session$setInputs(file = data.frame(
      name = "sample.csv", datapath = path, size = file.info(path)$size,
      type = "text/csv", stringsAsFactors = FALSE
    ))
    session$flushReact()
    session$flushReact()

    session$setInputs(
      player_col = "Player", date_col = "Date", matchday_col = "Matchday",
      task_col = "Task", duration_col = "Duration",
      metric_col = c("Total Distance (m)", "Speed (km/h)"),
      metric_overview = "Total Distance (m)",
      date_overview = as.Date(c("2026-01-01", "2026-01-02")),
      metric_trend = "Total Distance (m)", date_trend = as.Date(c("2026-01-01", "2026-01-02")),
      metric_match = "Total Distance (m)", metric_task = "Total Distance (m)",
      metric_session = "Total Distance (m)", session_date = as.Date("2026-01-02"),
      metric_readiness = "Total Distance (m)", z_window = 3, acwr_acute = 3, acwr_chronic = 5,
      metric_micro = "Total Distance (m)", micro_anchor = as.Date("2026-01-02"),
      micro_match_window = 2, micro_training_days = 3,
      profile_x = "Total Distance (m)", profile_y = "Speed (km/h)", profile_date = as.Date("2026-01-02")
    )
    session$flushReact()
    session$setInputs(apply_mapping = 1)
    session$flushReact()

    expect_equal(applied_mapping()$player, "Player")
    expect_equal(applied_mapping()$date, "Date")
    expect_setequal(applied_metrics(), c("Total Distance (m)", "Speed (km/h)"))
    expect_equal(nrow(base_data()), 4)
    expect_true(inherits(output$overview_plot, "json"))
    expect_true(nchar(output$overview_plot) > 100)
    for (id in c("trend_plot", "matchday_plot", "task_plot", "session_plot",
                 "readiness_z_plot", "readiness_acwr_plot", "microcycle_plot", "quadrant_plot")) {
      expect_true(inherits(output[[id]], "json"), info = id)
    }
  })
})
