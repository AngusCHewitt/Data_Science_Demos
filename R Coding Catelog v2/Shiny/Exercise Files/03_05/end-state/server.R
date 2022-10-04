library("tidyverse")
library("leaflet")
library("leaflet.extras")
library("rnaturalearthdata")
library("sf")

countries_sf <- countries110 %>%
  st_as_sf()

function(input, output, session) {
  
  observeEvent(input$selected_country_1,
               {
                 updateSelectInput(session,
                                   "selected_country_2",
                                   selected = input$selected_country_1)
               })
  
  observeEvent(input$selected_country_2,
               {
                 updateSelectInput(session,
                                   "selected_country_1",
                                   selected = input$selected_country_2)
               })
  
  
  output$highlighted_world_map_1 <- renderLeaflet({
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
          filter(name == input$selected_country_1),
        fillColor = "red",
        fillOpacity = 1,
        weight = 1,
        color = "black"
      ) %>%
      leaflet.extras::setMapWidgetStyle(style = list(background = "#0ca4ff"))
    
  })
  
  output$highlighted_world_map_2 <- renderLeaflet({
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
          filter(name == input$selected_country_2),
        fillColor = "red",
        fillOpacity = 1,
        weight = 1,
        color = "black"
      ) %>%
      leaflet.extras::setMapWidgetStyle(style = list(background = "#0ca4ff"))
    
  })
  
}