library("tidyverse")

wdi_data <- read_csv("data/wdi_data.csv") %>%
  filter(!str_detect(country, "^A"))

function(input, output, session) {
  
  updateSelectInput(session,
                    "select_country_with_updateSelectInput",
                    choices = unique(wdi_data$country))
  
  output$internet_use_without_updateSelectInput <- renderPlot({
    wdi_data %>%
      filter(
        country == input$select_country_without_updateSelectInput,
        indicator == "IT.NET.USER.ZS"
      ) %>%
      filter(!is.na(value)) %>%
      ggplot(aes(x = year, y = value)) +
      geom_path() +
      labs(
        title = paste("Individuals using the Internet (% of population)", "in", input$select_country_without_updateSelectInput),
        subtitle = "Data source: WDI Package, see data/world-bank.R for details"
      )
  })
  
  output$internet_use_with_updateSelectInput <- renderPlot({
    wdi_data %>%
      filter(
        country == input$select_country_with_updateSelectInput,
        indicator == "IT.NET.USER.ZS"
      ) %>%
      filter(!is.na(value)) %>%
      ggplot(aes(x = year, y = value)) +
      geom_path() +
      labs(
        title = paste("Individuals using the Internet (% of population)", "in", input$select_country_with_updateSelectInput),
        subtitle = "Data source: WDI Package, see data/world-bank.R for details"
      )
  })
  
}