library("tidyverse")

wdi_data <- read_csv("data/wdi_data.csv")

countries_and_continents <- read_csv("data/countries.csv") %>%
  filter(name %in% wdi_data$country)

indicators <- read_csv("data/indicators.csv")

function(input, output, session) {
  
  updateSelectInput(session,
                    "selected_continent",
                    choices = unique(countries_and_continents$continent))
  
  observeEvent(input$selected_continent,
               {
                 countries_in_continent <- countries_and_continents %>%
                   filter(continent == input$selected_continent) %>%
                   pull(name)
                 
                 updateSelectInput(session,
                                   "selected_country",
                                   choices = countries_in_continent)
                 
               })
  
  updateSelectInput(session,
                    "selected_indicator",
                    choices = setNames(indicators$indicator_code, indicators$indicator_name))
  
  
  output$wdi_indicator_chart <- renderPlot({
    
    selected_indicator_name <- indicators %>%
      filter(indicator_code == input$selected_indicator) %>%
      pull(indicator_name)
    
    wdi_data %>%
      filter(
        country == input$selected_country
      ) %>%
      filter(indicator == input$selected_indicator) %>%
      filter(!is.na(value)) %>%
      ggplot(aes(x = year, y = value)) +
      geom_path() +
      labs(
        title = paste("Individuals using the Internet (% of population)", "in", input$selected_country),
        subtitle = "Data source: WDI Package, see data/world-bank.R for details"
      )
  })
  
}