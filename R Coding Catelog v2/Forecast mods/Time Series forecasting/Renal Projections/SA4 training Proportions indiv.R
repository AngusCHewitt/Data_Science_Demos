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

# deterministic mods
trend <- seq_along(training_ds )
(fit1 <- auto.arima(training_ds , d=0, xreg=trend))

# stocastic mods
trend <- seq_along(training_ds )
(fit2 <- auto.arima(training_ds , d=1))


fc1 <- forecast(fit1,
  xreg = cbind(trend = 11:13))

fc2 <- forecast(fit2, h=3)

accuracy(fc1,test_ds)
accuracy(fc2,test_ds)

autoplot(training_ds) +
  autolayer(fc2, series="Stochastic trend") +
  autolayer(fc1, series="Deterministic trend")



