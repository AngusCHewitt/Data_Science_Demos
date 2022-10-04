library(shiny)

ui <- fluidPage(
  sliderInput("mean",
              label = "Mean",
              min = 1,
              max = 2,
              value = 1,
              step = 0.1),
  plotOutput("hist")
)

server <- function(input, output, session) {
  
  output$hist <- renderPlot(
    hist(rnorm(50, mean = input$mean), 
         xlim = c(-5, 5))
  )
  
}

shinyApp(ui, server)