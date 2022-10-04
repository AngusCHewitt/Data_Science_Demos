library(qcc)
library(tidyverse)
library(fable)
library(tsibble)
library(lubridate)

##--- App1 fcast 3 different moels, accuarcy summary stats, fcast vs catuals chart of the best model (ets)
##-- summarystats out indicates the best mdoel.
##-- need to drill into 2 tab to het a better look at model performance using the 3 performance charts

##-- example forecast and use contro chart to monitor the performance of the accuaracy of the fcast
data("AirPassengers")

##-- convwet tsibble
AirPassengers %>%
  as_tsibble() -> AirPassengers_tb 

##-- train data
training_dt <- subset(AirPassengers_tb, index < ymd("1960,01,01"))

##-- test data
test_dt <- subset(AirPassengers_tb,index >= ymd("1960,01,01"))

##-- 3 3 fcoast mods to training dt
##-- store obj for app
training_dt %>%
  model(ets = ETS(value ~ season("M") + trend("A")),
        ar = AR(value),
        arima = ARIMA(value)) %>%
  forecast(h = 12L) -> fc


##---------------- 3 outputs for Tab 1 ----------------##
##-- plot fcast and training actusl
fc %>%
autoplot(training_dt)

##-- calc MAPE for test dt
accuracy(fc, test_dt)

#-- join fcasted values with actuals to show visual comaprison betwene proj v actuals
#1.2 % MAPE
fc %>%
  filter(.model == "ets") %>%
  rename(forecast  = ".mean",
         forecast_distn  = "value")  %>%
  select(-.model) -> fc_subset

##- left join best fitted training fcast with actuals
AirPassengers_tb %>%
  left_join(fc_subset, by = "index") -> fc_With_Actuals

##00 visualise acutals vs forecast
fc_With_Actuals %>%
  ggplot(aes(x = index, y = value)) + geom_line(alpha = .2, size = 1) +  geom_line(aes(y = forecast), color = "red", size = 1.5, alpha = .2) +
  ggtitle("Training Forecast vs Actuals (forecast in red)") + labs(x = "Arrival months", y = "Monthly presentations") + theme_classic()



##---------------- 3 performance chart in the 2 tab ----------------##

fc_MAPE = (abs(test_dt$value - fc_subset$forecast)/test_dt$value)*100

##-- place forecast in control cahrt
control_Chart_dt <- qcc(fc_MAPE, type = "xbar.one", center = 0)
cumsum_dt <- cusum(fc_MAPE, center = 0, decision.interval = 5)
ewma_dt <- ewma(fc_MAPE, center = 0)

##-- dataset for all the 3 performance chatsrs and differ
perf_Chart_dts <- tibble(date = fc_subset$index,
                             AirPassengers_actuals = test_dt$value,
                             diff_Actuals_fcast = fc_MAPE,
                             control_Chart_LCL = control_Chart_dt$limits[1],
                             control_Chart_UCL = control_Chart_dt$limits[2],
                             center = 0,
                             ewma_Smooth = ewma_dt$y,
                             ewma_LCL = ewma_dt$limits[,1],
                             ewma_UCL = ewma_dt$limits[,2],
                             cusum_pos = cumsum_dt$pos,
                             cusum_neg = cumsum_dt$neg,
                             cusum_UCL = 5,
                             cusum_LCL = -5)


##-- control chart
perf_Chart_dts %>%
  ggplot(aes(x = date, y = diff_Actuals_fcast)) + geom_line(alpha = .2, size = 1) + geom_point(alpha = .5, size = 2) +
  geom_line(aes(y = control_Chart_LCL), color = "red", size = 1.5, alpha = .2) + geom_line(aes(y = control_Chart_UCL), color = "red", size = 1.5, alpha = .2) + 
  geom_line(aes(y = center), color = "black", size = 1.5, alpha = .2) + 
  ggtitle("") + labs(x = "Date", y = "MAPE") + theme_classic() -> p 

##-- turn ggplot into interactive visual
plotly::ggplotly(p)



##-- EWMA
perf_Chart_dts %>%
  ggplot(aes(x = date, y = diff_Actuals_fcast)) + geom_point(size = 2, shape = 3, color = "black") +
  geom_line(aes(y = ewma_Smooth), color = "red", size = 1.5, alpha = .2) + geom_line(aes(y = ewma_LCL), color = "red", size = 1.5, alpha = .2) + 
  geom_line(aes(y = ewma_UCL), color = "red", size = 1.5, alpha = .2) + 
  geom_line(aes(y = center), color = "black", size = 1.5, alpha = .2) + 
  ggtitle("") + labs(x = "Date", y = "MAPE") + theme_classic() -> p 

##-- turn ggplot into interactive visual
plotly::ggplotly(p)



##-- cusum
perf_Chart_dts %>%
  ggplot(aes(x = date, y = cusum_pos)) + geom_line(size = 1.5) +  
  geom_line(aes(y = cusum_UCL), color = "red", size = 1.5, alpha = .2) + 
  geom_line(aes(y = cusum_LCL), color = "red", size = 1.5, alpha = .2) + 
  geom_line(aes(y = center), color = "black", size = 1.5, alpha = .2) + 
  ggtitle("") + labs(x = "Date", y = "MAPE") + theme_classic() -> p 

##-- turn ggplot into interactive visual
plotly::ggplotly(p)
