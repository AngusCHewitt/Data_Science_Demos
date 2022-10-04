library("tidyverse")
library("shinyjs")

function(input, output, session){
  
  runcodeServer()
  
  output$curve_plot <- renderPlot({
    
    curve(x^input$exponent, from = -5, to = 5)
    
  })
  
}