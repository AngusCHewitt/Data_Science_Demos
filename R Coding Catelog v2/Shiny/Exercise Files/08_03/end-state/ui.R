library("shinyjs")
fluidPage(
  useShinyjs(),
  runcodeUI(type = "textarea"),
  selectInput("exponent",
              "Exponent:",
              choices = 1:5),
  plotOutput("curve_plot")
)