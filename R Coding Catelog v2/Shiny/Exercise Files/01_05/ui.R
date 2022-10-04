library("DT")

fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_continent",
                  label = "Select a continent",
                  choices = NULL
      ),
      selectInput("selected_year",
                  label = "Select a year",
                  choices = NULL
      )
    ),
    mainPanel(  dataTableOutput("species_table"))
  )
)