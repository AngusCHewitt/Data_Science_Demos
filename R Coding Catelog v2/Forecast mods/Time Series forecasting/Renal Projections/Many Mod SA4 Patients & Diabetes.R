# Many mods with ANZ Diabtetes dataset

# Examine Dataset and coerce into a nested dataset;

# Modelling SA4 reates - use gapminder package to handle many mods

library("modelr")
library("tidyverse")
library("gapminder")
library("forecast")
library("tidyquant")
library("timetk")
library("sweep")


# Goal forecast number of renal patients for each SA4;
## Can accuarte forecast be created using the ANZDATA dataset

# sa4 dataset with diebetes
dataset <- read.csv("SA4 Mod split by Diabetes.csv")

str(dataset)


#-- Use gapminder package to dataset with nest col. grouped by (SA4 Diabetes) --#

# nest data of each year to create a flat file (by SA4, Diabetes)
by_SA4 <- dataset %>% 
  group_by(SA4,Diabetes) %>% 
  nest()
by_SA4$data[[1]]


# unnest residuals - so can visualise
distrn <- unnest(by_SA4, data)
distrn

# visualise t.s. of each SA4 & Diebetes combination 
distrn %>% 
  ggplot(aes(x=Year, y=Renal_Dialysis,colour=Diabetes)) + facet_wrap(~distrn$SA4) + geom_point() + geom_line()



