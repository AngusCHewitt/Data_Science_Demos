library("DT")

navbarPage(
  "Interdependent Controls | % of populatiion using the internet",
  tabPanel(
    "Countries by Continent",
    fluidPage(fluidRow(
      column(
        selectInput("selected_continent",
                    "Select a continent",
                    choices = NULL),
        width = 6
      ),
      column(
        selectInput("selected_country",
                    "Select a country",
                    choices = NULL),
        width = 6
      )
    )),
    plotOutput("internet_use_chart")
  ),
  collapsible = TRUE
)