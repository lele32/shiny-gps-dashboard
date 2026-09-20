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

source("R/gps_helpers.R", local = environment())
source("R/gps_semantics.R", local = environment())
source("R/lift_ui.R", local = environment())
source("R/lift_server.R", local = environment())

server <- gps_server
shinyApp(ui, server)
