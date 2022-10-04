# testing matrices and arrays 


a# t.s. decomp and forecast

library(forecast)
library(seasonal)
library(ggplot2)

plot(AirPassengers)

# create training and test datasets

training <- window(AirPassengers,end=c(1959,12))
test <- window(AirPassengers,start=c(1960,1))

# stablise the variance with logs
log_training <- log(training) 

# auto forcast log of Airpassenger
f_log_tr <- forecast(log_training,h=12)

checkresiduals(f_log_tr)

# return to normal scale with exp
exp_for <- exp(f_log_tr$mean)

# error summary stats - 2nd argumwent = actuals 
accuracy(exp_for,test)

# visualise the diff between actual and forecast
autoplot(test) + autolayer(exp_for)

# examine residuals
plot(exp_for-test,type="o")

# create d.f. with t.s. and month no.
data_AirPass <- data.frame(Passenger=AirPassengers,Month_no=seq(1:12))

# review march t.s. 
data <- subset(data_AirPass,Month_no==3,Passenger)

# create t.s. objects for trainign and test subsets
march_ts <- ts(data$Passenger,start=1949,frequency=1)
march_test <- window(march_ts,end=1959)
march_tr <- window(march_ts,start=1960)

# test tslm forecast
test_log <- forecast(fit,h=2)

fit <- tslm(march_test~trend) # best mod

checkresiduals(fit) # tslm residuals test and visuals 


