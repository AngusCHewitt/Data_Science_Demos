# Weather Data from BOM

#install.packages("bomrang")

# Weather station Directory
# http://www.bom.gov.au/climate/data/stations/

library(bomrang)
library(tidyverse)
library(lubridate)

##-- Date from 2013
##-- finds nearest weather station
Melbourne <- get_historical(
  stationid = NULL,
  latlon = c(-37.84, 144.98),
  radius = NULL,
  type = c("rain", "min", "max", "solar")
)

##-- Date from 1970
Melbourne_Airport <- get_historical(
  stationid = NULL,
  latlon = c(-37.69, 144.84),
  radius = NULL,
  type = c("rain", "min", "max", "solar")
)


##-- Date from 1970
Gabba <- get_historical(
  stationid = NULL,
  latlon = c(-27.48, 153.03),
  radius = NULL,
  type = c("rain", "min", "max", "solar")
)




##-- Add up-to-date dates for Melb Weather
Melbourne_Airport %>%
  mutate(Date = make_date(year,month,day)) -> Melbourne_Airport


##-- reshape dataset to form around the prob of occurance for partiucular player type --##
library(tidyverse)
library(vcd)
library(RcmdrMisc)


# Careers features dataset
load("/Users/angushewitt/Desktop/AFL Datasets/Model Data/Step_6_AFL_Features_Dataset.RData")


##-- Just MCG 
AFL_Features_dt %>%
  filter(Venue == "M.C.G.") -> AFL_Features_dt_Melbourne_Airport


##-- Merge Rain Data 
AFL_Features_dt_Melbourne_Airport %>%
  left_join(Melbourne_Airport, by = "Date") -> AFL_Features_dt_Melbourne_Airport


AFL_Features_dt_Melbourne_Airport %>%
 mutate(rain_binary = if_else(rainfall >  0,"Rain","No_Rain")) -> AFL_Features_dt_Melbourne_Airport

library(vcd)
##-- The Rest has more prob HGS with less number favs
Table <- xtabs(Highest_Goal_Scorer == "HGS" ~ rain_binary + The_Rest_Categories, data = AFL_Features_dt_Melbourne_Airport)
mosaic(Table, shade = TRUE,rot_labels=c(0,0,0,0))

glm <- glm(HGS_Binary ~ rainfall:The_Rest_Categories, data = AFL_Features_dt_Melbourne_Airport, family = binomial(link = "logit"))

›

set.seed(1001)  
# perform k means cluster analysis
clus_ds <- kmeans(AFL_Features_dt_Melbourne_Airport$rainfall, centers = 4, nstart = 50)


clus_ranks = data.frame(clusters = 1:4, ranks = rank(clus_ds$centers))

##-- add height cluster components to model
AFL_Features_dt_Melbourne_Airport %>%
  mutate(Rain_Clust = clus_ds$cluster) %>%
  left_join(clus_ranks, by = c("Rain_Clust" = "clusters")) %>%
  mutate(RainCategories =              case_when(ranks == 1 ~ "No Rain",
                                                   ranks == 2 ~ "Av Rain",
                                                   ranks == 3 ~ "Above Av Rain",
                                                   ranks == 4 ~ "Heavy Rain")) -> AFL_Features_dt_Melbourne_Airport


# corece into an order factor, ordinal factor
AFL_Features_dt_Melbourne_Airport$RainCategories <- with(AFL_Features_dt_Melbourne_Airport, 
                                                             factor(RainCategories, levels=c("No Rain", "Av Rain", "Above Av Rain", "Heavy Rain"), 
                                                                    ordered=TRUE))



##-- The Rest has more prob HGS with less number favs
Table <- xtabs(Highest_Goal_Scorer == "HGS" ~ RainCategories + The_Rest_Categories, data = AFL_Features_dt_Melbourne_Airport)
mosaic(Table, shade = TRUE,rot_labels=c(0,0,0,0))





# reproduce
set.seed(108)
# function to compute total within-cluster sum of square 
wss_ratio <- function(k) {
  data = kmeans(AFL_Features_dt_Gabba$rainfall, k, nstart = 20 )
  
  ratio = data$tot.withinss / data$totss
}

# Compute and plot wss for k = 1 to k = 10
k.values <- 1:10

# extract wss for 1-10 clusters
wss_ratio <- map_dbl(k.values, wss_ratio)
plot(wss_ratio,type = "o")

  

