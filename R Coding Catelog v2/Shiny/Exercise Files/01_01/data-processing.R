library(tidyverse)

countries <- read_csv("data/countries.csv")

indicators <- read_csv("data/indicators.csv")


countries_sf <- countries110 %>%
  st_as_sf()

get_wdi_indicator <- function(country, indicator) {
  WDI(
    country = country,
    indicator = c("value" = indicator)
  ) %>%
    as_tibble()
}

get_indicator_name <- function(selected_indicator) {
  indicators %>%
    filter(indicator_code == selected_indicator) %>%
    pull(indicator_name)
}

get_country_name <- function(selected_country) {
  countries %>%
    filter(iso2c == selected_country) %>%
    pull(name)
}


gg_wdi_indicator_line <- function(country, indicator) {
  wdi_data <- get_wdi_indicator(
    country,
    indicator
  )

  selected_indicator_name <- get_indicator_name(indicator)

  selected_country_name <- get_country_name(country)

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
}
