# build fcast combinations function

# short list of auto fcasts
fcast_list <- function (df,h = 12) { 
  
  require(tidyverse)
  require(forecast)
  
  # auto. fcast list  pipe to fcast function
  ETS <- ets(df) %>% forecast(h = h) 
  ARIMA <- auto.arima(log(df)) %>% forecast(h = h) 
  
  # distrn's
  fcast_list <- list(ETS = ETS,
                     AUTO_ARIMA = ARIMA)
  
  
  fcast_list(ets)
  
  # iterate list of functions
  fit <- mthly[1:3] %>% map(ets)
  
  map(fit,forecast,h=24)
  
  # point est. 
  fcast_pest <- map(fcast_list, "mean")
  
  {
    fcast_pest # output
  }
  
}
