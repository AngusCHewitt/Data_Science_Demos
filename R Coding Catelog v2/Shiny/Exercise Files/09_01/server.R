library("leaflet")
library("tidyverse")
library("rnaturalearthdata")
library("sf")
library("gapminder")

countries_sf <- countries110 %>%
  st_as_sf()
colnames(countries_sf) <- tolower(colnames(countries_sf))
countries_sf <- countries_sf %>%
  filter(!name == "Antarctica")

function(input, output, session){
  
  updateSelectInput(session,
                    "selected_continent",
                    choices = unique(countries_sf$continent))
  
  output$world_map <- renderLeaflet({
    
    if(input$selected_continent == ""){
      return()
    }
    
      leaflet() %>%
      addPolygons(data = countries_sf %>%
                    filter(continent == input$selected_continent),
                  fillColor = "red",
                  color = "black",
                  weight = 1) %>%
      addPolygons(data = countries_sf %>%
                    filter(!continent == input$selected_continent),
                  fillColor = "blue",
                  color = "black",
                  weight = 1)
    
  })
  
}