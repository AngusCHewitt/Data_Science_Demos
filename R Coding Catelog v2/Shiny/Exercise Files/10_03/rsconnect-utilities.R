library("rsconnect")
library("tidyverse")

connected_accounts <- accounts()

apps_in_account <- function(account_name){
  
  applications(account_name) %>%
    as_tibble() %>%
    mutate_all(list(~unlist(.)))
  
}

all_shiny_apps <- connected_accounts %>%
  mutate(apps = map(name, ~apps_in_account(.x))) %>%
  unnest(apps) %>%
  as_tibble()

all_shiny_apps
