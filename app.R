library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("GPS Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload GPS Data")
    ),
    mainPanel(
      textOutput("status")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  output$status <- renderText({
    if (is.null(input$file)) {
      return("No file uploaded yet.")
    } else {
      return(paste("File uploaded:", input$file$name))
    }
  })
}

# Run the app
shinyApp(ui, server)

