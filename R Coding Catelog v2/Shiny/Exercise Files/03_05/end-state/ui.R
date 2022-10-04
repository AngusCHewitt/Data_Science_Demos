library("leaflet")

navbarPage(
  "Don't duplicate inputs/outputs",
  tabPanel("Analysis 1",
           fluidPage(
             div(
               selectInput("selected_country_1",
                           "Selected country:",
                           choices = c("Russia",
                                       "Egypt",
                                       "Australia",
                                       "Canada",
                                       "Algeria"
                           )),
               style = "position:relative;z-index:10000;"
             ),
             leafletOutput("highlighted_world_map_1")
           )),
  tabPanel("World map and indicator",
           fluidPage(
             div(
               selectInput("selected_country_2",
                           "Selected country:",
                           choices = c("Russia",
                                       "Egypt",
                                       "Australia",
                                       "Canada",
                                       "Algeria"
                           )),
               style = "position:relative;z-index:10000;"
             ),
             leafletOutput("highlighted_world_map_2")
           )),
  collapsible = TRUE
)