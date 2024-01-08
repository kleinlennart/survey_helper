library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #loading {
        position: fixed;
        top: 50%;
        left: 50%;
        margin-top: -50px;
        margin-left: -100px;
        background-color: white;
        padding: 10px;
        z-index: 100;
      }
    "))
  ),
  div(id = "loading", "Loading..."),
  actionButton("load", "Load Data"),
  plotOutput("plot")
)

server <- function(input, output, session) {

  # Simulate some data loading
  observeEvent(input$load, {
    Sys.sleep(2)  # simulate a time-consuming operation
    output$plot <- renderPlot({
      plot(mtcars$mpg, mtcars$wt)
    })

    # Hide the loading screen using JavaScript
    session$sendCustomMessage(type = 'hide-loading', message = 'loaded')
  })
}

# Include the JavaScript to hide the loading screen
shinyApp(
  ui = ui,
  server = server,
  options = list(
    jsHandlers = list(
      'hide-loading' = I('$("#loading").hide();')
    )
  )
)
