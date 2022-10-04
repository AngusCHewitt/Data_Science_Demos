library("shinycustomloader")
navbarPage(
  "shinycustomloader | Spinners when downloading data",
  tabPanel(
    "WDI Indicator",
    fluidPage(
      fluidRow(
        column(selectInput("selected_country",
                           "Selected country:",
                           choices = NULL),
               width = 6),
        column(selectInput("selected_indicator",
                           "Selected indicator",
                           choices = NULL,
                           width = "100%"),
               width = 6)
      ),
      actionButton("update_chart",
                   "Update Chart",
                   width = "100%"),
      withLoader(plotOutput("wdi_chart"))
    )
  ),
  collapsible = TRUE
)