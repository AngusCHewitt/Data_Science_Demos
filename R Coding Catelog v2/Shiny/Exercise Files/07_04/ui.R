library("DT")
library("shinyjs")
library("rhandsontable")

navbarPage(
  "Capture input with rhandsontable | Time Tracker",
  tabPanel(
    "Upload data",
    fluidPage(
      
      wellPanel("Log your time in the interactive table below, and then click 'Analyse'"),
      rHandsontableOutput("time_tracker_rhandsontable"),
      br(),
      actionButton("analyse_data",
                   "Analyse Data",
                   width = "100%"),
      plotOutput("time_tracker_summary")
    )
  ),
  collapsible = TRUE
)