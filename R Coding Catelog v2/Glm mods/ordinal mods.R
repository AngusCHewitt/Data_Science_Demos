library(ordinal)
library(tidyverse)

## A simple cumulative link model:
fm1 <- clm(rating ~ contact + temp, data=wine)

glimpse(wine)


