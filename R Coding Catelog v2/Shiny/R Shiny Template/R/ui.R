# Define UI for shiny app

navbarPageWithLogo(
  title = "COVID-19 Analytics Template",
  logo = tags$img(id = "navbar-logo", src = "DH Logo White.svg"),
  inverse = T,
  position = "fixed-top",
  tabPanel(
    "Tab 1",
    tags$head(includeCSS("styles.css")),
    sidebarLayout(
      sidebarPanel(
        id = "sidebar",
        h3("Sidebar")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        style = "margin-top:64px",
        h3("Main panel")
      )
    )
  ),
  aboutUI("about")
)