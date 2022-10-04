# Modelling SA4 reates - use gapminder package to handle many mods

library("modelr")
library("tidyverse")
library("gapminder")
library("forecast")

# sa4 dataset
dataset <- read.csv("ANZDATA by SA4 - Actuals and Rates.csv")

# test for Hume
dataset %>% 
  filter(SA4 == "Geelong" & Year < 2012) -> Geelong


dataset %>% 
  filter(SA4 == "Geelong" & Year > 2011) -> New_Geelong


new = data.frame(Year=2012:2017,Prop_Diabetes=New_Geelong$Prop_Diabetes)

ts_Geelong <- ts(Geelong,start=2005,frequency=1)

fit2 <- tslm(Patients ~ Year:Prop_Diabetes,data=ts_Geelong)

fcast <- forecast(fit2,h=5,newdata=new)


summary(fit2)

#-- Use gapminder package to create mod for each SA4 --#

# nest data of each year to create a flat file (by country)
by_SA4 <- dataset %>% 
  group_by(SA4) %>% 
  nest()
by_SA4$data[[1]]


# create linear mod using nested dataset var. 
SA4_model <- function(df) {
  lm(Patients ~ Year data = df)
}


# pass nest column throu. linear function
models <- map(by_SA4$data, SA4_model)


# Store mod as a in the nest dataset - related info to each var store together
by_SA4 <- by_SA4 %>% 
mutate(model = map(data, SA4_model))
by_SA4

# create a var. for residuals
by_SA4 <- by_SA4 %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_SA4

# unnest residuals - so can visualise
resids <- unnest(by_SA4, resids)
resids

# visualise all sa4 residuals
resids %>% 
  ggplot(aes(Year, resid)) +
  geom_line(aes(group = SA4), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

# add lin mod stats to nested d.s.
by_SA4 %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance)

# unnest mod stats
glance <- by_SA4 %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
glance

# sort by r -square stat.
glance %>% 
  arrange(adj.r.squared)


glance %>% 
  ggplot(aes( SA4, adj.r.squared)) + 
  geom_point()





