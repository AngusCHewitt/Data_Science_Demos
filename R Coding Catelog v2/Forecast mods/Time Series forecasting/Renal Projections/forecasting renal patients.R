# t.s. and foprecasting analysis of renal patients at SA4 level

library(forecast)

# load d.s.
Dataset <- read.table("/Users/angushewitt/Downloads/SA4 ANZDATA.csv", header=TRUE, sep=",", 
  na.strings="NA", dec=".", strip.white=TRUE)

# ballart renal patients
ts_ballart <- ts(Dataset$Ballarat,start=2005,frequency=1)

plot(diff(ts_ballart))

training_ts <- window(ts_ballart,end=2014)

test_ts <- window(ts_ballart,start=2015)

# Deterministic Trend
trend <- seq_along(training_ts)

(fit1 <- auto.arima(training_ts, d=1, xreg=trend))

plot(forecast(fit1,xreg = cbind(trend = 11:13),h=3)

projection <- forecast(fit1,xreg = cbind(trend = 11:13),h=3)

accuracy(projection ,test_ts)

# Stocastic mods
(fit2 <- auto.arima(training_ts, d=1))

projection2 <- forecast(fit2,h=3)

accuracy(projection2 ,test_ts)

plot(forecast(fit2,h=3))

# modeling diffs

diffs <- diff(training_ts)

acf(diffs)

test <- forecast(diffs,h=3)