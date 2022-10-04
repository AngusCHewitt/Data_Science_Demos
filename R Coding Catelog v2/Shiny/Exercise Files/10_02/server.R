library("tidyverse")
library("WDI")
library("leaflet")
library("sf")
library("rnaturalearthdata")

source("data-processing.R", local = TRUE)

function(input, output, session) {
  updateSelectInput(session,
    "selected_continent",
    choices = unique(countries$continent)
  )

  observeEvent(input$selected_continent, {
    countries_in_continent <- countries %>%
      filter(continent == input$selected_continent)




    updateSelectInput(session,
      "selected_country",
      choices = setNames(countries_in_continent$iso2c, countries_in_continent$name)
    )
  })

  updateSelectInput(session,
    "selected_indicator",
    choices = setNames(indicators$indicator_code, indicators$indicator_name)
  )

  output$wdi_indicator_chart <- renderPlot({
    if (input$update_chart == 0) {
      return()
    }

    gg_wdi_indicator_line(
      isolate(input$selected_country),
      isolate(input$selected_indicator)
    )
  })

  output$highlighted_world_map <- renderLeaflet({
    if (input$update_chart == 0) {
      return()
    }

    countries_sf %>%
      filter(!name == "Antarctica") %>%
      leaflet() %>%
      addPolygons(
        fillColor = "grey",
        fillOpacity = 1,
        weight = 1,
        color = "black"
      ) %>%
      addPolygons(
        data = countries_sf %>%
          filter(iso_a2 == isolate(input$selected_country)),
        fillColor = "red",
        fillOpacity = 1,
        weight = 1,
        color = "black"
      ) %>%
      leaflet.extras::setMapWidgetStyle(style = list(background = "#0ca4ff"))
  })
}
