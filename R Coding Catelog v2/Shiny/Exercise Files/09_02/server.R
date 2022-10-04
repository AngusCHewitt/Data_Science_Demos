
function(input, output, session){

  output$shiny_logo <- renderImage(
    {list(src = "images/shiny-logo.png",
          width = "150px")},
    deleteFile = FALSE
  )

}