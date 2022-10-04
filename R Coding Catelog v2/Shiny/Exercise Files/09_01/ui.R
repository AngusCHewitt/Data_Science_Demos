library("leaflet")

navbarPage(
  "Custom CSS | Fixing Leaflet",
  tabPanel(
    "Map",
    fluidPage(
      wellPanel(
        h4("About this app"),
        p("This app uses custom CSS in two locations:"),
        p(" - div() wrapper"),
        p(" - theme argument of navbarPage")
      ),
      selectInput("selected_continent",
                  "Selected continent",
                  choices = NULL),
      leafletOutput("world_map")
    )
  ),
  collapsible = TRUE
)