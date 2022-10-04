library("tidyverse")
library("DT")
library("gapminder")

function(input, output, session) {
  updateSelectInput(session,
    "selected_continent",
    choices = unique(gapminder$continent)
  )

  observeEvent(input$selected_continent, {
    available_years <- gapminder %>%
      filter(continent == input$selected_continent) %>%
      select(year) %>%
      unique() %>%
      .[[1]]

    updateSelectInput(session,
      "selected_year",
      choices = available_years
    )
  })

  output$species_table <- renderDataTable({
    gapminder %>%
      filter(continent == input$selected_continent) %>%
      filter(year == input$selected_year) %>%
      select(-continent, -year) %>%
      datatable(rownames = FALSE)
  })
}