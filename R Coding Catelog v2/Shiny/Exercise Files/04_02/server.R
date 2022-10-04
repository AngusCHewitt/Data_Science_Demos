library("tidyverse")

function(input, output, session){
  
  output$star_narrow <- renderTable({
    
    starwars %>%
      select(name, species, homeworld, height) %>%
      filter(height <= input$height_limit_star_narrow) %>%
      arrange(desc(height))
    
  })
  
  output$star_wide <- renderTable({
    
    starwars %>%
      select(name:species) %>%
      filter(height <= input$height_limit_star_wide) %>%
      arrange(desc(height))
    
  })
  
  output$star_lists <- renderTable({
    
    starwars %>%
      select(name, films, vehicles, starships) %>%
      unnest()
    
  }, width = "100%")
  
}