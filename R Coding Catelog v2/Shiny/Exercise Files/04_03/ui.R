navbarPage(
  "shiny::renderTable",
  tabPanel(
    "Star Narrow",
    fluidPage(
      tableOutput("star_narrow")
    )
  ),
  tabPanel(
    "Star Wide",
    fluidPage(
      tableOutput("star_wide")
    )
  ),
  tabPanel(
    "Star List",
    fluidPage(
      tableOutput("star_lists")
    )
  ),
  collapsible = TRUE
)