library("shinycustomloader")
library("leaflet")

navbarPage(
  "What is Shiny?",
  tabPanel(
    "World Bank Indicator Dataviz",
    fluidPage(
      wellPanel(
        h4("What is this?"),
        p("This is an example Shiny app that visualises data from the World Data Bank using the WDI package."),
        p("The controls you can see are written in R code and the visualisations that are displayed are written in R. The entire interface and behaviour of the web application is written in R, using the Shiny package.")
      ),
      div(
        fluidRow(
          column(
            selectInput("selected_continent",
              "Select a continent",
              choices = NULL
            ),
            width = 3
          ),
          column(
            selectInput("selected_country",
              "Select a country",
              choices = NULL
            ),
            width = 4
          ),
          column(
            selectInput("selected_indicator",
              "Select an indicator",
              choices = NULL,
              width = "100%"
            ),
            width = 5
          )
        ),
        style = "position:relative;z-index:10000;"
      )
    ),
    actionButton("update_chart",
      "Update Chart",
      width = "100%"
    ),
    hr(),
    fluidRow(
      column(
        withLoader(leafletOutput("highlighted_world_map"), loader = "dnaspin"),
        width = 6
      ),
      column(
        withLoader(plotOutput("wdi_indicator_chart"), loader = "dnaspin"),
        width = 6
      )
    )
  ),
  collapsible = TRUE
)
