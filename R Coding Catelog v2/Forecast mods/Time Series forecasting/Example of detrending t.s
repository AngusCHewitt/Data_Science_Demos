# SA4 project proportions

library(forecast)
library(tidyverse)

Dataset <-   readXL("//internal.vic.gov.au/DHHS/HomeDirs6/ahew2812/Documents/SA4 Ballarat Props.xlsx",
   rownames=FALSE, header=TRUE, na="", sheet="Sheet1", stringsAsFactors=TRUE)

str(Dataset)


# t.s. series dataset
ts_data <-  ts(Dataset$Ballarat,start=2005,frequency = 1)

plot(training_ds)

#training dataset
training_ds <- window(ts_data,end=c(2014))

# test dataset for each obs
test_ds <- window(ts_data,start=c(2015))

# plot training diffs
plot(diff(training_ds))

fit <- loess(Ballarat ~ Year,data=Dataset)

#loess_curve
trend <- fit$fitted

# plot detrended data
plot(ts_data - trend)

detrended <- ts_data - trend

for_trend <-forecast(trend,h=8)
for_rand <- forecast(detrended,h=8)

for_trend$mean + for_rand$mean 

# subset training data
Dataset %>% 
filter(Year < 2015) -> training

cor(Dataset$Year,Dataset$Ballarat )

# trend component of the training mod
fit_tr <- loess(Ballarat ~ Year,data=training)
trend_tr <- fit_tr $fitted

training <- ts(data=training[,2],start=2005,frequency=1)

detrended_tr <- training$Ballarat - trend_tr

trend_proj <- holt(trend_tr ,h=3)
trend_proj2 <- forecast(trend_tr ,h=3)
detrend_proj <- forecast(detrended_tr,h=3)

plot(holt(trend_tr))

proj_tr <- trend_proj$mean + detrend_proj$mean

autoplot(training ) +
  autolayer(proj_tr, series="Holt's method") 

