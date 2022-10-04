library("tidyverse")
library("WDI")

source("data-processing.R", local = TRUE)

function(input, output, session) {
  updateSelectInput(session,
    "selected_country",
    choices = setNames(countries$iso2c, countries$name)
  )

  updateSelectInput(session,
    "selected_indicator",
    choices = setNames(indicators$indicator_code, indicators$indicator_name)
  )

  output$wdi_chart <- renderPlot({
    if (input$update_chart == 0) {
      return()
    }

    wdi_data <- get_wdi_indicator(
      isolate(input$selected_country),
      isolate(input$selected_indicator)
    )

    selected_indicator_name <- get_indicator_name(isolate(input$selected_indicator))

    selected_country_name <- get_country_name(isolate(input$selected_country))

    wdi_data %>%
      filter(!is.na(value)) %>%
      ggplot(aes(
        x = year,
        y = value
      )) +
      geom_line() +
      labs(
        title = paste(selected_indicator_name, "in", selected_country_name),
        subtitle = "Data source: WDI Package, see data/world-bank.R for details"
      )
  })
}
