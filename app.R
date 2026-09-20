# GPS LIFT / modular Shiny entrypoint --------------------------------------
#
# Keep the runtime small and explicit: source parsing, canonical GPS
# semantics, UI and server are separate modules. Deployment is intentionally
# not performed from this file.

suppressPackageStartupMessages({
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
})

options(shiny.maxRequestSize = 500 * 1024^2)

# Resolve modules from this entrypoint, not from the caller's working
# directory. This matters when RStudio launches the app from a project root,
# an open script folder or a restored session with another getwd().
gps_app_root <- local({
  source_file <- NULL
  for (frame_index in rev(seq_along(sys.frames()))) {
    candidate <- tryCatch(sys.frame(frame_index)$ofile, error = function(error) NULL)
    if (is.character(candidate) && length(candidate) == 1L && nzchar(candidate)) {
      source_file <- candidate
      break
    }
  }
  if (is.null(source_file)) getwd() else dirname(normalizePath(source_file, mustWork = FALSE))
})

source(file.path(gps_app_root, "R", "gps_helpers.R"), local = environment())
source(file.path(gps_app_root, "R", "gps_semantics.R"), local = environment())
source(file.path(gps_app_root, "R", "lift_ui.R"), local = environment())
source(file.path(gps_app_root, "R", "lift_server.R"), local = environment())

server <- gps_server
shinyApp(ui, server)
