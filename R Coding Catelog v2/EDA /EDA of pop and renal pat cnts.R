##-- explore relationship between pop and renal counts --##
library(tidyverse)
library(forecast)
library(ForecastComb)

nest_combine_ds <- readRDS("//internal.vic.gov.au/DHHS/HomeDirs6/ahew2812/Desktop/R Code/nest_combine_ds.rds")

# explore correlation with pop and LGA's
nest_combine_ds %>%
  mutate(cor = map(data, function(x) cor(x$patient_counts,x$LGA_Pop))) %>%
  unnest(cor) -> test

ggplot(data = test, aes(x = 1:79, y =  cor)) + geom_point()

dat_bal <-  data.frame(test$data[45])

plot(dat_bal$patient_counts,type = "o")