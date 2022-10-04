library("tidyverse")
library("rsconnect")

"the_shiny_app/"

apps_tib <- tribble(
  ~appDir, ~appName,
  "the_shiny_app", "foobar-app",
  "the_shiny_app", "fubar-app"
)

deploy_app <- function(name_of_app,
                       directory_of_app){
  
  tryCatch(
    deployApp(
      account = "visibledata",
      appDir = "the_shiny_app",
      appName = "programmatic-deploy",
      forceUpdate = TRUE
    ),
    error = function(e) {
      FALSE
    },
    finally = TRUE)
  
}

apps_tib %>%
  mutate(deployed = map_lgl(list(appDir, appName), ~deploy_app(.x, .y)))
