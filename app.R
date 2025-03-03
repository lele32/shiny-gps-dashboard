library(shiny)
library(readr)     # For reading CSV files
library(readxl)    # For reading Excel files
library(jsonlite)  # For reading JSON files
library(DT)        # For rendering interactive tables

# Define UI
ui <- fluidPage(
  titlePanel("GPS Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload GPS Data",
                accept = c(".csv", ".xlsx", ".json")),
      tags$hr(),
      uiOutput("file_info") # Dynamic file info
    ),
    mainPanel(
      DTOutput("table") # Interactive data table
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Function to read data based on file type
  read_data <- reactive({
    req(input$file)  # Ensures a file is uploaded before proceeding
    
    ext <- tools::file_ext(input$file$name)
    file_path <- input$file$datapath
    
    switch(ext,
           "csv" = read_csv(file_path, show_col_types = FALSE),
           "xlsx" = read_excel(file_path),
           "json" = fromJSON(file_path, flatten = TRUE),
           stop("Unsupported file type. Please upload CSV, XLSX, or JSON.")
    )
  })
  
  # Display uploaded file details
  output$file_info <- renderUI({
    req(input$file)
    HTML(paste0("<b>File Name:</b> ", input$file$name, "<br>",
                "<b>File Size:</b> ", round(input$file$size / 1024, 2), " KB"))
  })
  
  # Render table output
  output$table <- renderDT({
    req(read_data())  # Ensures data is available
    datatable(read_data(), options = list(pageLength = 10))
  })
}

# Run the app
shinyApp(ui, server)
