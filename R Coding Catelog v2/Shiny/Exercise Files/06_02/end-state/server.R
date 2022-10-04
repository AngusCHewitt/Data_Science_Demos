library("tidyverse")

wdi_data <- read_csv("data/wdi_data.csv")

indicators <- read_csv("data/indicators.csv")

function(input, output, session) {
  
  updateSelectInput(session,
                    "select_country_with_manual_labels",
                    choices = unique(wdi_data$country)
  )
  
  updateSelectInput(session,
                    "select_country_with_data_labels",
                    choices = unique(wdi_data$country)
  )
  
  updateSelectInput(session,
                    "selected_indicator_labelled_from_data",
                    choices = setNames(indicators$indicator_code,
                                       indicators$indicator_name))
  
  output$chart_manual_labels <- renderPlot({
    wdi_data %>%
      filter(
        country == input$select_country_with_manual_labels
      ) %>%
      filter(indicator == input$selected_indicator_labelled_manual) %>%
      filter(!is.na(value)) %>%
      ggplot(aes(x = year, y = value)) +
      geom_path() +
      labs(
        title = paste(
          input$selected_indicator_labelled_manual,
          "in",
          input$select_country_with_manual_labels
        ),
        subtitle = "Data source: WDI Package, see data/world-bank.R for details"
      )
  })

  output$chart_data_labels <- renderPlot({
    
    selected_indicator_name <- indicators %>%
      filter(indicator_code == input$selected_indicator_labelled_from_data) %>%
      pull(indicator_name)

    wdi_data %>%
      filter(
        country == input$select_country_with_data_labels
      ) %>%
      filter(indicator == input$selected_indicator_labelled_from_data) %>%
      filter(!is.na(value)) %>%
      ggplot(aes(x = year, y = value)) +
      geom_path() +
      labs(
        title = paste(
          selected_indicator_name,
          "in",
          input$select_country_with_data_labels
        ),
        subtitle = "Data source: WDI Package, see data/world-bank.R for details"
      )

  })
}