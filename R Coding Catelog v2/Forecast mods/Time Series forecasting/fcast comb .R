#-- Forecast combinations 
# goal: use Functional prog to forecast each statistical area using a weighted 
# ... combinations of auto forecast + tslm (renal ~ pop:year)

# 1: list of fcast functions using the pmap.
# 2: pmap(training d.s., fcasts) -> output tibble of univariant fcasts
# 3: pmap(fcast_list, ForecastCombo) -> output tibble of fcast weights for each uni. t.s.
# 4: function for combining Optimal weighted combinations multipied by training d.s.

#-- Optional
# 5: How will accuracy be incorped in this process
# 6: some visuals or use of features could be useful here
# 7: tslm models could be incorped later when SA3 data is available ~ tslm (renal ~ pop:year)

##-- obj: create functions that can complete the above steps

# 1: list of fcast functions using the pmap.

library(tidyverse)
library(forecast)
library(ForecastComb)

# test forecast combination soptimisation
## OLS to find best fitted weigths for each coefficient - reduce residuals



# list fcast methods
methods(forecast)

# m3 mthly d.s.
mthly <- subset(Mcomp::M3, "monthly")

# t.s. split into hst and future data
mthly[[1]]["x"] # training - hist
mthly[[1]]["xx"] # test - future


library(M3)

M1.yearly.industry <- subset(Mcomp::M3,1,"industry")

M1.yearly.industry

# goal: from auto forecats need to store forecast means in matrices 

# 1: create training & test sets to feed into forecast function
train_beer <- window(fpp2::ausbeer,end = c(1999,4)) %>% as.vector()
test_beer <- window(fpp2::ausbeer,start = c(2000,1)) %>% as.vector()

# need to ensure dims and t.s. freq. are equal
train_length <- length(train_beer) 
test_length <- length(fpp2::ausbeer) - length(train_beer)

# 2: create training & test sets to feed into daat comb
ts_tr_beer <- window(fpp2::ausbeer,end = c(1999,4))
ts_test_beer <- window(fpp2::ausbeer,start = c(2000,1))

# 3: fcast training set
ETS <- ets(ts_tr_beer) %>% forecast(h = test_length)
BATS <- bats(ts_tr_beer) %>% forecast(h = test_length)
ARIMA <- auto.arima(log(ts_tr_beer)) %>% forecast(h = test_length) 

# 4: coerce fcast into a matrix - col for each fcast method
test_fcast <- matrix(c(ETS$mean, BATS$mean, exp(ARIMA$mean)), nrow = test_length, ncol = 3)

# 5: training predictions - mod fits - length and dims of training
train_fcast <- matrix(cbind(fitted(ETS),fitted(BATS),exp(fitted(ARIMA))), nrow = train_length, ncol = 3)

# 6: combine all the test, training  with the actual t.s. dataset, ensure lengths are equal
data <- foreccomb(train_beer, train_fcast, test_beer, test_fcast)

# 7: combine fcasts methods to min. criterion
best_combo <- auto_combine(data, criterion = "RMSE")


best_fcast.ts <- ts(best_combo$Forecasts_Test, start = c(2000,1), frequency = 4)


autoplot(ts_test_beer) + autolayer(best_fcast.ts)




