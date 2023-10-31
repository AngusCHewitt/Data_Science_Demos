library(shiny)
library(shinydashboard)#Rshinydashboardfuns
library(shinyWidgets)#CSSandJavafuns
library(plotly) # interactive plots
library(tidyverse) # tody dt
library(splines) # ns vars 
library(randomNames)


load("data/app_Sample_dt.rds") # sample dt 
load("mod/diabetes_polr_mod.rds") # plyr mod 



## create fiction names 
set.seed(123)
tb_Lookup_dt <- app_Dt %>%
  mutate(fictional_Names = randomNames(100))

tb_Lookup_dt %>%
  select(-BMI, -GenHlth:-PhysHlth, -Age:-ordinal_Diabetes) %>%
  gather("var", "value", -ID, -fictional_Names) %>%
  mutate(value = if_else(value == 1, "True", "False")) %>%
  spread(var, value) -> tb_Lookup_dt

glimpse(tb_Lookup_dt)

datatable(tb_Lookup_dt) %>% formatStyle(
  c("DiffWalk"),
  target = 'cell',
  backgroundColor = styleEqual(c("True", "False"), c('#e5f5f9', "#fc9272")))