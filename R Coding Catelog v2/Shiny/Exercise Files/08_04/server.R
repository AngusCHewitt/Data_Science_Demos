library("tidyverse")

function(input, output, session){
  
  hist_data <- eventReactive(c(input$mean,
                               input$standard_deviation),
                             
                             rnorm(100, mean = input$mean,
                                   sd = input$standard_deviation)
                             
                             )
  
  output$hist <- renderPlot({
    
    if(input$update_hist == 0){
      return()
    }
    
    hist_data <- isolate(hist_data())
    
    hist(hist_data,
         xlim = c(-5, 5))
    
  })
  
}