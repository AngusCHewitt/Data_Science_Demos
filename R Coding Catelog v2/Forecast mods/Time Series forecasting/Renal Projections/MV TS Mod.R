# Many mods with ANZ Diabtetes dataset

# Modelling SA4 reates - use gapminder package to handle many mods

library("modelr")
library("tidyverse")
library("gapminder")
library("forecast")
library("lubridate")


# sa4 dataset
dataset <- read_csv("SA4 Mod split by Diabetes.csv")

str(dataset)


#-- Use gapminder package to create mod for each SA4 --#

# nest data of each year to create a flat file (by SA4, Diabetes)
by_SA4 <- dataset %>% 
  group_by(SA4,Diabetes) %>% 
  nest()
by_SA4$data[[1]]


# create linear mod using nested dataset var. 
SA4_model <- function(df) {
  lm(Renal_Dialysis ~ Scaled_Pop, data = df)}


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

# visualise all sa4 residuals factted by SA4
resids %>% 
  ggplot(aes(x=Scaled_Pop, y=Renal_Dialysis,colour=Diabetes)) + facet_wrap(~resids$SA4) + geom_point() + geom_line()


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
  ggplot(aes( x=SA4, y=adj.r.squared,colour=Diabetes)) + 
  geom_point(size=3,alpha=0.5)

View(glance)






