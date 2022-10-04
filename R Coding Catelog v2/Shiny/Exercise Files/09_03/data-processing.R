
countries <- read_csv("data/countries.csv")

indicators <- read_csv("data/indicators.csv")

get_wdi_indicator <- function(country, indicator) {
  Sys.sleep(3)
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
