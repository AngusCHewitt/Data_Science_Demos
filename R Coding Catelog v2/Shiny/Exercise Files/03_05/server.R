library("tidyverse")
library("leaflet")
library("leaflet.extras")
library("rnaturalearthdata")
library("sf")

countries_sf <- countries110 %>%
  st_as_sf()

function(input, output, session) {
  
  
  output$highlighted_world_map <- renderLeaflet({
    countries_sf %>%
      leaflet() %>%
      addPolygons(
        fillColor = "grey",
        fillOpacity = 1,
        weight = 1,
        color = "black"
      ) %>%
      addPolygons(
        data = countries_sf %>%
          filter(name == input$selected_country),
        fillColor = "red",
        fillOpacity = 1,
        weight = 1,
        color = "black"
      ) %>%
      leaflet.extras::setMapWidgetStyle(style = list(background = "#0ca4ff"))
    
  })
  
}