library("DT")

navbarPage(
  "Labeling menu-items | % of population using the internet",
  tabPanel(
    "Manual labels",
    fluidPage(
      fluidRow(
        column(
          selectInput(
            "select_country_with_manual_labels",
            label = "Select a country",
            choices = NULL
          ),
          width = 6
        ),
        column(
          selectInput(
            "selected_indicator_labelled_manual",
            "Select an indicator to display",
            choices = c(
              "Individuals using the Internet (% of population)" = "IT.NET.USER.ZS",
              "Commercial bank branches (per 100,000 adults)" = "FB.CBK.BRCH.P5",
              "Secondary education, pupils" = "SE.SEC.ENRL"
            ),
            width = "100%"
          ),
          width = 6
        )
      ),
      plotOutput("chart_manual_labels")
    )
  ),
  tabPanel(
    "Labels from data",
    fluidPage(
      fluidRow(
        column(
          selectInput(
            "select_country_with_data_labels",
            label = "Select a country",
            choices = NULL
          ),
          width = 6
        ),
        column(
          selectInput(
            "selected_indicator_labelled_from_data",
            "Select an indicator to display",
            choices = NULL,
            width = "100%"
          ),
          width = 6
        )
      ),
      plotOutput("chart_data_labels")
    )
  ),
  collapsible = TRUE
)
