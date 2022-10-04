library("tidyverse")

wdi_data <- read_csv("data/wdi_data.csv")

countries_and_continents <- read_csv("data/countries.csv") %>%
  rename(country = name)

function(input, output, session) {
  
  updateSelectInput(session,
                    "selected_continent",
                    choices = unique(countries_and_continents$continent))
  
  updateSelectInput(session,
                    "selected_country",
                    choices = unique(countries_and_continents$country))
  
  output$internet_use_chart <- renderPlot({
    
    wdi_data %>%
      filter(
        country == input$selected_country,
        indicator == "IT.NET.USER.ZS"
      ) %>%
      filter(!is.na(value)) %>%
      ggplot(aes(x = year, y = value)) +
      geom_path() +
      labs(
        title = paste("Individuals using the Internet (% of population)", "in", input$selected_country),
        subtitle = "Data source: WDI Package, see data/world-bank.R for details"
      )
  })
  
}