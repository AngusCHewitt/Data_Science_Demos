
library("modelr")
library("tidyverse")
library("gapminder")
library("forecast")
library("tidyquant")
library("timetk")
library("sweep")
library("lubridate")
library("broom")

## -- Use the Broom Pgk to keep related mod info with mod data

dataset <- read_csv("//internal.vic.gov.au/DHHS/HomeDirs6/ahew2812/Documents/ANZDATA SA3 Renal Diaylsis Patients with DELWP Pop.csv")

# plot line chart of all SA3's - Patients no. vs no. Years 

str(dataset)

# visual t.s.
dataset %>% 
  mutate(SA3  = as.factor(SA3 )) %>%
  ggplot(aes(x = Pop, y = Renal_Patients, colour = SA3 )) +
  geom_line() + theme_classic() + theme(legend.position="none") + ggtitle("No. renal dialysis patients vs Pop at Each SA3 (2006-2017)")

# flatten file to each row rep an SA3 - tibble nested with SA3 var. 
by_SA3 <- dataset %>%
  mutate(SA3 = as.factor(SA3)) %>%
  group_by(SA3,SA4,Location) %>% 
  nest()
by_SA3$data[[1]]

# Build a function to pass through nested dataset using the gapminder toolkit
# Lineat Mod Function explanatory vars - Year & Pop Interaction

SA3_model <- function(df) {
  lm(Renal_Patients ~ Year:Pop, data = df)
}

# Use the broom package to collect mod quality info.  
# 1: create the mod of each SA3 
# 2: add related mod info. with broom functions  
# 3: Use map to iterate all elements of glance "adjust r square all SA3's  

by_SA3  <- by_SA3 %>%  
  mutate (model_trend = map(data, SA3_model),
         mod_para = model_trend %>% map(broom::tidy),
         mod_quality = model_trend %>% map(broom::glance),
         mod_obs_level = model_trend %>% map(broom::augment),
         adj.r.square = mod_quality %>% map_dbl("adj.r.squared"))       
   


