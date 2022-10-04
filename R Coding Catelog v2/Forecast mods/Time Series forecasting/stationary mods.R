# decompose / diff / transform t.s. and model stationary t.s. 
 
 library(forecast)
 #library(M3)
 library(tidyverse)
 
 # predict no. air line passengers for next year
 
 plot(AirPassengers)
 
 # multiplicataive seasons, strong positive trend
 
 # create training and test t.s. (cross validaiton)
 
 AP_training.ts <- window(AirPassengers,end=c(1959,12)) 
 
 AP_test.ts <- window(AirPassengers,start=c(1960,1)) 

 # smooth monthly dataset to account for the number of days in month 
 smooth.ts <- AP_training.ts/monthdays(AP_training.ts)

 #x11 decompose well suited to monthly and qtrly data
 
library(seasonal)
smooth.ts %>% seas(x11="") -> fit
autoplot(fit)

# seasonal compoents
seas <- seasonal(fit)

# trend compents
trend <- trendcycle(fit)

# remainder
res <- remainder(fit)

trend = trend * monthdays(trend)

seas %>% forecast(h=12) -> proj_seas 

trend %>% forecast(h=12) -> proj_trend 

res %>% forecast(h=12) -> proj_res 

proj_ds <- cbind(trend=proj_trend$mean,seasonal=proj_seas$mean,res=proj_res$mean)


# multiplicative seasons with linear trend and residual 
forecast <- (proj_ds[,1]*proj_ds[,2]) + proj_ds[,3]

mean(abs( (forecast-AP_test.ts)/AP_test.ts)) # MAPE= 3%