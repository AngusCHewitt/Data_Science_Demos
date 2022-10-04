# linear with ARMIA Errors

library(forecast)

fit <- Arima(Dataset$Sales,xreg=Dataset$Obs,order=c(1,1,0))

acf(diff(Dataset$Sales))

acf(fit$residuals)

plot(fit)

nobs <- as.data.frame(cbind(Obs=14:23))

plot(forecast(fit,h=10,xreg= nobs))

window() # create test and training datasets

#expo smooth

# test dataset length
test_length <- round(length(AirPassengers) * .2,0)

training <- subset(AirPassengers,end=length(AirPassengers)-test_length)
test <- subset(AirPassengers,start=length(AirPassengers)-test_length)


fit1 <- ets(training ) #expo smooth
fit2 <- hw(training,seasonal="additive") # holts additive 
fit3 <- hw(training,seasonal="multiplicative") # holts multi

summary(fit1)

checkresiduals(ets_AP)

# have to run forecast model for ets 
ets_AP <- forecast(fit1,h=29) 

accuracy(ets_AP,test)
accuracy(fit2,test)
accuracy(fit3,test)

# plot all the competing forecasts
autoplot(AirPassengers) +
  autolayer(ets_AP, series="ETS", PI=FALSE) +
  autolayer(fit2, series="HOLTS Add", PI=FALSE) +
  autolayer(fit3, series="HOLTS Multi", PI=FALSE)
  
  #-- adusting for days in a month --#s
  
  # smooth monthly t.s. by the number of day
  dframe <- cbind(Monthly = AirPassengers,
                DailyAverage = AirPassengers/monthdays(AirPassengers))
  autoplot(dframe, facet=TRUE)
  
  # forecast smooth t.s.
  
   DailyAverage = AirPassengers/monthdays(AirPassengers)
  
  # test dataset length
Day_length <- round(length(DailyAverage) * .2,0)

training_day <- subset(  DailyAverage ,end=length(AirPassengers)-Day_length)
test_day <- subset(  DailyAverage ,start=length(AirPassengers)-Day_length)


fit4 <- ets(training_day ) #expo smooth

ets_AP <- forecast(fit4,h=29) 

fit5 <- hw(training_day,seasonal="additive") # holts additive 
fit6 <- hw(training_day,seasonal="multiplicative") # holts multi

new_data <- ts(ets_AP[2]) 

new_data * monthdays(test_day)

accuracy(ets_AP,test_day)
accuracy(fit5,test_day)
accuracy(fit6,test_day)

# plot all the competing forecasts
autoplot(DailyAverage) +
  autolayer(ets_AP, series="ETS", PI=FALSE) +
  autolayer(fit5, series="HOLTS Add", PI=FALSE) +
  autolayer(fit6, series="HOLTS Multi", PI=FALSE)

  
  