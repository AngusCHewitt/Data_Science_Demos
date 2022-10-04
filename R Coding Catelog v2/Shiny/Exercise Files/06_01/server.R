library("tidyverse")

wdi_data <- read_csv("data/wdi_data.csv")

function(input, output, session) {
  
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
  
}