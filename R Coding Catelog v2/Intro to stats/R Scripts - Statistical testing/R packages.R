library(rvest) 
library(magrittr)
library(tidyverse)
library(plotly)

table <- read_html("https://cran.r-project.org/web/packages/available_packages_by_date.html")
 
table %>%
  html_node("table") %>%
  html_table() %>% # create d.f. from html table
  mutate(Date = as.Date(Date)) %>%
  mutate(Month = format(Date, format="%Y-%m")) %>%
  mutate(Packages = rev(1:nrow(.))) -> pkgs

plot_ly(data=pkgs,x=~Date,y=~count)