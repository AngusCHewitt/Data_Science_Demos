library("leaflet")

navbarPage(
  "Custom CSS | Fixing Leaflet",
  theme = "custom.css",
  tabPanel(
    "Map",
    fluidPage(
      wellPanel(
        h4("About this app"),
        p("This app uses custom CSS in two locations:"),
        p(" - div() wrapper"),
        p(" - theme argument of navbarPage")
      ),
      div(selectInput("selected_continent",
                      "Selected continent",
                      choices = NULL),
          style = "position:relative;z-index:10000"),
      leafletOutput("world_map")
    )
  ),
  collapsible = TRUE
)