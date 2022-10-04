fluidPage(
  sliderInput("mean",
              "Mean",
              min = -2,
              max = 2,
              value = 0),
  sliderInput("standard_deviation",
              "Standard deviation",
              min = 1,
              max = 3,
              value = 1,
              step = 0.2),
  actionButton("update_hist",
               "Update histogram"),
  plotOutput("hist")
)