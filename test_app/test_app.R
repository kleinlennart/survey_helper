library(shiny)

ui <- fluidPage(
  selectInput("dataChoice", "Choose Dataset:", choices = c("None", "mtcars", "iris")),
  tableOutput("table")
)

server <- function(input, output) {
  # Reactive expression for table data
  tableData <- reactive({
    if (input$dataChoice == "None") {
      # Default dataset or a placeholder
      data.frame(Message = "Please select a dataset")
    } else if (input$dataChoice == "mtcars") {
      mtcars
    } else if (input$dataChoice == "iris") {
      iris
    }
  })

  # Render the table
  output$table <- renderTable({
    tableData()
  })
}

shinyApp(ui, server)
