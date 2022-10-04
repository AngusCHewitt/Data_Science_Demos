library("DT")

fluidPage(
  h1("World Bank Data"),
  wellPanel("This Shiny app includes three .csv files created by the following script; data/world-bank.R"),
  DTOutput("wdi_table")
)
