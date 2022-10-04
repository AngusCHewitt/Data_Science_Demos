# forecast each month renal registry age bracket

library(forecast)
library(tidyverse)
library(tscount)

getwd()

setwd()

Dataset <- read.csv("~/monthly Renal Registry - 10 year intervals.csv")

str(Dataset)

attach(Dataset)


# all training t.s
Dataset %>%
filter(Obs<121) -> Training_ds  

# all test t.s
Dataset %>%
  filter(Obs>=121) -> test_ds

# months test & training
month_training <- Training_ds$Month_number
month_test <- test_ds$Month_number


# year test & training
year_training <- Training_ds$year.data
year_test <- test_ds$year.data


# trend test & training
trend_training <- Training_ds$Obs
trend_test <- test_ds$Obs


#*-- Female Projections --*#

#-- females 0-14 years of age --#

# display t.s.
tsdisplay(diff(Female0_14)) 


# training and test t.s.
training_f0_14 <- Training_ds$Female0_14
test_f0_14 <- test_ds$Female0_14

# benchmark forecast - females 0 to 14 with low counts distrn
training_f0_14 %>% croston(h=30) -> benchmark

# covariants with months

acf(diff(training_f0_14),lag=50)
pacf(diff(training_f0_14),lag=50)

# no covariant
fit_pois1 <- tsglm(training_f0_14, model = list(past_obs = 1, past_mean = 13),
distr = "poisson")# best Mod

fit_pois2 <- tsglm(training_f0_14, model = list(past_obs = 1, past_mean = 8),
                       distr = "poisson") 


train_mod1 <- predict(fit_pois1, n.ahead = 30, level = 0.9)

train_mod2 <- predict(fit_pois2, n.ahead = 30, level = 0.9)

# check mod accuracy
accuracy(train_mod1$median,test_f0_14)# mod1
accuracy(train_mod2$median,test_f0_14)# mod2
accuracy(benchmark$mean,test_f0_14)# benchmark

# project counts from 2018 - to Dec 2025
project_pois <- tsglm(Female0_14, model = list(past_obs = 1, past_mean = 13),
                        distr = "poisson") # best Mod

# forecast females 0-14 to Dec 2025
project_mod1 <- predict(project_pois , n.ahead = 98, level = 0.9)

# forecast d.s
forecast_Female0_14 <- data.frame(Female0_14 = project_mod1$median)


#-- females 15-24 years of age --#

# display t.s
tsdisplay(Female15_24)

acf(diff(Female15_24),lag=50)
pacf(diff(Female15_24),lag=50)

# training and test t.s.
training_f15_24 <- Training_ds$Female15_24
test_f15_24 <- test_ds$Female15_24

# benchmark forecast - females 15 to 24 with low counts distrn
training_f15_24 %>% croston(h=30) -> benchmark

# no covariant
fit_pois1 <- tsglm(training_f15_24, model = list(past_obs = 1, past_mean = 12),
            distr = "poisson")

fit_pois2 <- tsglm(training_f15_24, model = list(past_obs = 1, past_mean = 22),
              distr = "poisson") # best Mod


train_mod1 <- predict(fit_pois1, n.ahead = 30, level = 0.9)

train_mod2 <- predict(fit_pois2, n.ahead = 30, level = 0.9)



# check mod accuracy
accuracy(train_mod1$median,test_f15_24)# mod1
accuracy(train_mod2$median,test_f15_24)# mod2
accuracy(benchmark$mean,test_f15_24)# benchmark


# forecast - females 15 to 24 with low counts distrn - to Dec 2026
Female15_24 %>% croston(h=98) -> benchmark_forecast


# forecast d.s
forecast_Female15_24 <- data.frame(Female15_24 = round(benchmark_forecast$mean,0))

#-- females 25 - 34 years of age --#

# display t.s.
tsdisplay(diff(Female25_34)) 


# training and test t.s.
training_f25_34 <- Training_ds$Female25_34
test_f25_34 <- test_ds$Female25_34

# benchmark forecast - females 0 to 14 with low counts distrn
training_f25_34 %>% croston(h=30) -> benchmark

# covariants with months

acf(diff(training_f25_34),lag=50)
pacf(diff(training_f25_34),lag=50)

# no covariant
fit_pois1 <- tsglm(training_f25_34, model = list(past_obs = c(1,8,9), past_mean = 10),
                   distr = "poisson") # best Mod

fit_pois2 <- tsglm(training_f25_34, model = list(past_obs = c(1,9), past_mean = 12),
                   distr = "poisson")


train_mod1 <- predict(fit_pois1, n.ahead = 30, level = 0.9)

train_mod2 <- predict(fit_pois2, n.ahead = 30, level = 0.9)

# check mod accuracy
accuracy(train_mod1$median,test_f25_34)# mod1
accuracy(train_mod2$median,test_f25_34)# mod2
accuracy(benchmark$mean,test_f25_34)# benchmark

# project counts from 2018 - to Dec 2025
project_pois <- tsglm(Female25_34, model = list(past_obs = c(1,8,9), past_mean = 10),
                      distr = "poisson") # best Mod

acf(diff(Female25_34))

# forecast females24-35 to Dec 2025
project_mod1 <- predict(project_pois , n.ahead = 98, level = 0.9)

# forecast d.s
forecast_Female25_34 <- data.frame(Female25_34 = project_mod1$median)


#-- females 34 - 44 years of age --#

# display t.s.
tsdisplay(diff(Female35_44)) 
tsdisplay(Female35_44) 


# training and test t.s.
training_f35_44 <- Training_ds$Female35_44
test_f35_44 <- test_ds$Female35_44

# benchmark forecast - females 0 to 14 with low counts distrn
training_f35_44 %>% croston(h=30) -> benchmark

# covariants with months

acf(diff(training_f35_44),lag=50)
pacf(diff(training_f35_44),lag=50)

ts_f34_44 <- ts(training_f35_44,start=c(2006,05),frequency =12 )  

# decomp t.s. into t,s, and res.
plot(decompose(ts_f34_44))

# no covariant
fit_pois1 <- tsglm(ts_f34_44, model = list(past_obs = 1, past_mean = 6),
                   distr = "poisson")# best mod 

fit_pois2 <- tsglm(ts_f34_44, model = list(past_obs = 1, past_mean = 9),
                   distr = "poisson")

auto_for <-  stlf(ts_f34_44,h=30)


train_mod1 <- predict(fit_pois1, n.ahead = 30, level = 0.9)

train_mod2 <- predict(fit_pois2, n.ahead = 30, level = 0.9)


# check mod accuracy
accuracy(train_mod1$median,test_f35_44)# mod1
accuracy(train_mod2$median,test_f35_44)# mod2
accuracy(auto_for,test_f35_44)# mod2
accuracy(benchmark$mean,test_f35_44)# benchmark

# project counts from 2018 - to Dec 2025
project_pois <- tsglm(Female35_44, model = list(past_obs = 1, past_mean = 6),
                      distr = "poisson") # best Mod



checkresiduals(fit_pois1$residuals)
acf(diff(fit_pois2$residuals))



# forecast females24-35 to Dec 2025
project_mod1 <- predict(project_pois , n.ahead = 98, level = 0.9)

# create ts for forecast and actuals
ts_proj <- ts(project_mod1$median,start=c(2018,11),frequency =12)
data_ts <- ts(Female35_44,start=c(2006,05),frequency =12)

# plot actuals and projections
autoplot(data_ts) + autolayer( ts_proj,series="tscount") 
  

# forecast d.s
forecast_Female35_44 <- data.frame(Female34_44 = project_mod1$median)


#-- females 45 - 54 years of age --#

# display t.s.
tsdisplay(Female45_54) 
tsdisplay(diff(Female45_54),lag=50) 


# training and test t.s.
training_f45_54 <- Training_ds$Female45_54
test_f45_54 <- test_ds$Female45_54

# benchmark forecast - females 0 to 14 with low counts distrn
training_f45_54  %>% croston(h=30) -> benchmark


# covariants with months

acf(diff(training_f45_54),lag=50)
pacf(diff(training_f45_54),lag=50)

ts_f45_54 <- ts(training_f45_54,start=c(2006,05),frequency =12 )  

# decomp t.s. into t,s, and res.
plot(decompose(ts_f45_54))


# no covariant
fit_pois1 <- tsglm(ts_f45_54, model = list(past_obs = 1, past_mean = 4),xreg= trend_training,
                   distr = "poisson") 

fit_pois2 <- tsglm(ts_f45_54, model = list(past_obs = 1, past_mean = 19),xreg=trend_training,
                   distr = "poisson")


auto_for <-  stlf(ts_f45_54,h=30)# best mod


train_mod1 <- predict(fit_pois1, n.ahead = 30, level = 0.9)

train_mod2 <- predict(fit_pois2, n.ahead = 30, level = 0.9)


# check mod accuracy
accuracy(train_mod1$median,test_f45_54)# mod1
accuracy(train_mod2$median,test_f45_54)# mod2
accuracy(auto_for$mean,test_f45_54)# mod2
accuracy(benchmark$mean,test_f45_54)# benchmark

# create ts for forecast and actuals
data_ts <- ts(Female45_54,start=c(2006,05),frequency =12)

# forecast females45-54 to Dec 2025
project_mod1 <- stlf(data_ts, h=98)

checkresiduals(auto_for )

# plot actuals and projections
autoplot(data_ts) + autolayer( project_mod1$mean,series="stlf") 


# forecast d.s
forecast_Female45_54 <- data.frame(Female45_54 = round(project_mod1$mean,0))

#-- females 55 - 64 years of age --#

# display t.s.
tsdisplay(Female55_64) 
tsdisplay(diff(Female55_64),lag=50) 


# training and test t.s.
training_f55_64 <- Training_ds$Female55_64
test_f55_64 <- test_ds$Female55_64

# benchmark forecast - females 0 to 14 with low counts distrn
training_f55_64  %>% croston(h=30)-> benchmark


ts_f55_64 <- ts(training_f55_64,start=c(2006,05),frequency =12 )  

# decomp t.s. into t,s, and res.
plot(decompose(ts_f55_64))


plot(benchmark)


auto_for <- stlf(ts_f55_64,h=30,s.window=20,t.window=20,robust = TRUE )


# check mod accuracy
accuracy(auto_for$mean,test_f55_64)# mod2
accuracy(benchmark$mean,test_f55_64)# benchmark

# create ts for forecast and actuals
data_ts <- ts(Female55_64,start=c(2006,05),frequency =12)

# forecast females45-54 to Dec 2025
auto_for <- stlf(data_ts,h=98,s.window=20,t.window=20,robust = TRUE )

# plot actuals and projections
autoplot(data_ts) + autolayer( auto_for$mean,series="stlf_auto") 


# forecast d.s
forecast_Female55_64 <- data.frame(Female55_64 =auto_for$mean)


#-- females 65 - 74 years of age --#

# display t.s.
tsdisplay(Female65_74) 
tsdisplay(diff(Female65_74),lag=50) 

cor(Female65_74,Obs)

# training and test t.s.
training_f65_74 <- Training_ds$Female65_74
test_f65_74 <- test_ds$Female65_74

# benchmark forecast - females 0 to 14 with low counts distrn
training_f65_74  %>% croston(h=30) -> benchmark
  
  
ts_f65_74 <- ts(training_f65_74,start=c(2006,05),frequency =12 )  

# decomp t.s. into t,s, and res.
plot(decompose(ts_f65_74))

auto_for <- stlf(ts_f65_74,h=30,s.window=20,t.window=20,robust = TRUE )

plot(benchmark)

# no covariant
fit_pois1 <- tsglm(ts_f65_74, model = list(past_obs = 1, past_mean = 16),xreg=Training_ds$Obs,
                   distr = "poisson") 

fit_pois2 <- tsglm(ts_f65_74, model = list(past_obs = 1, past_mean = 13),xreg=Training_ds$Obs,
                   distr = "poisson")


auto_for <-  stlf(ts_f65_74,h=30)# best mod

train_mod1 <- predict(fit_pois1, n.ahead = 30, level = 0.9)
train_mod2 <- predict(fit_pois2, n.ahead = 30, level = 0.9)


# check mod accuracy
accuracy(train_mod1$median,test_f65_74)# mod1
accuracy(train_mod2$median,test_f65_74)# mod2
accuracy(auto_for,test_f65_74)# mod2
accuracy(benchmark$mean,test_f65_74)# benchmark

# create ts for forecast and actuals
data_ts <- ts(Female65_74,start=c(2006,05),frequency =12)


# forecast females45-54 to Dec 2025
project_mod1 <- stlf(data_ts, h=98,s.window=20,t.window=20,robust=TRUE)


checkresiduals(project_mod1 $residuals,lag=50 )

# plot actuals and projections
autoplot(data_ts) + autolayer( project_mod1$mean,series="tscount") 



# forecast d.s
forecast_Female65_74 <- data.frame(Female65_74 = project_mod1$mean)


#-- females 75 - 84 years of age --#

# display t.s.
tsdisplay(Female75_84) 
tsdisplay(diff(Female75_84),lag=50) 

cor(Female75_84,Obs)

# training and test t.s.
training_f75_84 <- Training_ds$Female75_84
test_f75_84 <- test_ds$Female75_84

# benchmark forecast - females 0 to 14 with low counts distrn
training_f75_84  %>% croston(h=30) -> benchmark


ts_f75_84 <- ts(training_f75_84,start=c(2006,05),frequency =12 )  

# decomp t.s. into t,s, and res.
plot(decompose(ts_f75_84))


plot(benchmark)


# training forecasts
auto_for <- stlf(ts_f75_84,h=30,s.window=20,t.window=20,robust = TRUE )

training_df <- ts(Training_ds$Obs,start=c(2006,5),frequency=12)

test <- auto.arima(ts_f75_84,xreg=training_df)
test2 <- forecast(test,h=30,xreg=121:150)


train_mod1 <- predict(fit_pois1, n.ahead = 30, level = 0.9,nxreg=trend_test)
train_mod2 <- predict(fit_pois2, n.ahead = 30, level = 0.9,nxreg=trend_test)


# check mod accuracy
accuracy(auto_for,test_f75_84)# mod2
accuracy(test2,test_f75_84)# benchmark


# plot actuals and projections
autoplot(ts(test_f75_84,start=c(2016,5),frequency=12)) + autolayer( auto_for,series="Auto", PI=FALSE) + autolayer( test2,series="ARIMA", PI=FALSE) 

# create ts for forecast and actuals
data_ts <- ts(Female75_84,start=c(2006,05),frequency =12)

plot(decompose(data_ts))

# forecast females45-54 to Dec 2025
project_mod1 <- stlf(data_ts, h=98,s.window=13,t.window=20,robust=TRUE)

checkresiduals(project_mod1 $residuals,lag=50 )

# plot actuals and projections
autoplot(data_ts) + autolayer( project_mod1$mean,series="stlf") 


# forecast d.s
forecast_Female75_84 <- data.frame(Female75_84 = project_mod1$mean)


#-- females 85+ --#

# display t.s.
tsdisplay(Female85.) 
tsdisplay(diff(Female85.),lag=50) 

cor(Female85.,Obs)

# training and test t.s.
training_f85. <- Training_ds$Female85.
test_f85. <- test_ds$Female85.

# benchmark forecast - females 0 to 14 with low counts distrn
training_f85.  %>% croston(h=30) -> benchmark


ts_f85. <- ts(training_f85.,start=c(2006,05),frequency =12 )  

# decomp t.s. into t,s, and res.
plot(decompose(ts_f85.))


plot(benchmark)

# no covariant
fit_pois1 <- tsglm(ts_f85., model = list(past_obs = 1,past_mean=40),xreg=Training_ds$Obs,
                   distr = "poisson") 

fit_pois2 <- tsglm(ts_f85., model = list(past_obs = 1, past_mean = 20),xreg=Training_ds$Obs,
                   distr = "poisson")

checkresiduals(fit_pois2)


# training forecasts
auto_for <- stlf(ts_f85.,h=30,s.window=20,t.window=20,robust = TRUE )

training_df <- ts(Training_ds$Obs,start=c(2006,5),frequency=12)

test <- auto.arima(ts_f85.,xreg=training_df)
test2 <- forecast(test,h=30,xreg=121:150)

summary(test2)

train_mod1 <- predict(fit_pois1, n.ahead = 30, level = 0.9,nxreg=trend_test)
train_mod2 <- predict(fit_pois2, n.ahead = 30, level = 0.9,nxreg=trend_test)


# check mod accuracy
accuracy(train_mod1$median,test_f85.)# mod1
accuracy(train_mod2$median,test_f85.)# mod2
accuracy(auto_for,test_f85.)# mod2
accuracy(test2,test_f85.)# benchmark


# plot actuals and projections
autoplot(ts(test_f85.,start=c(2016,5),frequency=12)) + autolayer( auto_for,series="Auto", PI=FALSE) + autolayer( test2,series="ARIMA", PI=FALSE) 

# create ts for forecast and actuals
data_ts <- ts(Female85.,start=c(2006,05),frequency =12)

plot(decompose(diff(data_ts)))

# forecast females85+ to Dec 2025
mod <- auto.arima(Female85.,xreg=Obs)
proj_mod <- forecast(mod,h=98,xreg=151:248)

nrow(as.data.frame(proj_mod))

checkresiduals(proj_mod $residuals,lag=50 )

# forecast d.s
forecast_Female85. <- data.frame(Female85. = proj_mod$mean)


#-- forecast output for Female Renial Patients --#

Projection_ds <- cbind(forecast_Female0_14,forecast_Female15_24,forecast_Female25_34,
                       forecast_Female35_44,forecast_Female45_54,forecast_Female55_64,
                       forecast_Female65_74,forecast_Female75_84,forecast_Female85.)


View(Projection_ds)

write.csv(Projection_ds,"Female_Projections.csv")




#*-- Male Projections --*#

#-- Males 0-14 years of age --#

# display t.s.
tsdisplay((Males0_14)) 



# training and test t.s.
training_M0_14 <- Training_ds$Males0_14
test_M0_14 <- test_ds$Males0_14


# benchmark forecast - females 0 to 14 with low counts distrn
training_M0_14 %>% croston(h=30) -> benchmark

# covariants with months

acf(diff(training_M0_14),lag=50)
pacf(diff(training_M0_14),lag=50)

# no covariant
fit_pois1 <- tsglm(training_M0_14, model = list(past_obs = 1, past_mean = 3),
                   distr = "poisson")# best Mod

fit_pois2 <- tsglm(training_M0_14, model = list(past_obs = c(1,3), past_mean = 8),
                   distr = "poisson") 


train_mod1 <- predict(fit_pois1, n.ahead = 30, level = 0.9)

train_mod2 <- predict(fit_pois2, n.ahead = 30, level = 0.9)

# check mod accuracy
accuracy(train_mod1$median,test_M0_14)# mod1
accuracy(train_mod2$median,test_M0_14)# mod2
accuracy(benchmark$mean,test_M0_14)# benchmark

# project counts from 2018 - to Dec 2025

# forecast MAkes 0-14 to Dec 2025
Males0_14 %>% croston(h=98) -> benchmark

plot(benchmark)

# forecast d.s
forecast_Male0_14 <- data.frame(Male0_14 = benchmark$mean)



#-- Males 15-24 years of age --#

# display t.s.
tsdisplay((Males15_24)) 
tsdisplay((diff(Males15_24))) 


# training and test t.s.
training_M15_24 <- Training_ds$Males15_24
test_M15_24 <- test_ds$Males15_24


# benchmark forecast - females 0 to 14 with low counts distrn
training_M15_24 %>% croston(h=30) -> benchmark

# covariants with months

pacf(diff(training_M15_24),lag=50)
acf(training_M15_24,lag=100)

# no covariant
fit_pois1 <- tsglm(training_M15_24, model = list(past_obs = 1, past_mean = 30),
                   distr = "poisson")# best Mod

fit_pois2 <- tsglm(training_M15_24, model = list(past_obs = 1, past_mean = 29),
                   distr = "poisson") 


train_mod1 <- predict(fit_pois1, n.ahead = 30, level = 0.9)

train_mod2 <- predict(fit_pois2, n.ahead = 30, level = 0.9)

# check mod accuracy
accuracy(train_mod1$median,test_M15_24)# mod1
accuracy(train_mod2$median,test_M15_24)# mod2
accuracy(benchmark$mean,test_M15_24)# benchmark

# project counts from 2018 - to Dec 2025

# forecast MAkes 0-14 to Dec 2025
Males15_24 %>% croston(h=98) -> benchmark

plot(benchmark)

# forecast d.s
forecast_Male15_24 <- data.frame(Male15_24 = benchmark$mean)


#-- Males 25-34 years of age --#

# display t.s.
tsdisplay((Males25_34)) 
tsdisplay((diff(Males25_34))) 

cor(Males25_34,Obs)

# training and test t.s.
training_M25_34 <- Training_ds$Males25_34
test_M25_34 <- test_ds$Males25_34


# benchmark forecast - females 0 to 14 with low counts distrn
training_M25_34 %>% croston(h=30) -> benchmark

# covariants with months

pacf(training_M25_34,lag=50)
acf(diff(training_M25_34),lag=100)

# no covariant
fit_pois1 <- tsglm(training_M25_34, model = list(past_obs = 1, past_mean = 10),
                   distr = "poisson")

fit_pois2 <- tsglm(training_M25_34, model = list(past_obs = 1, past_mean = 11),
                   distr = "poisson")# best Mod 


train_mod1 <- predict(fit_pois1, n.ahead = 30, level = 0.9)

train_mod2 <- predict(fit_pois2, n.ahead = 30, level = 0.9)

Males25_34_ts <- ts(training_M25_34,start=c(2006,05),frequency=12)

# automatted forecasts
auto1 <- forecast(training_M25_34,h=30)


# check mod accuracy
accuracy(train_mod1$median,test_M25_34)# mod1
accuracy(train_mod2$median,test_M25_34)# mod2
accuracy(auto1$mean,test_M25_34)# mod2
accuracy(benchmark$mean,test_M25_34)# benchmark

# project counts from 2018 - to Dec 2025

# forecast MAkes 0-14 to Dec 2025
fit_pois2 <- tsglm(Males25_34, model = list(past_obs = 1, past_mean = 11),
                   distr = "poisson")# best Mod 


mod2 <- predict(fit_pois2, n.ahead = 98, level = 0.9)

acf(fit_pois2$residuals,lag=50)

# forecast d.s
forecast_Male25_34 <- data.frame(Male25_34 = mod2$median)


#-- Males 35-44 years of age --#

# display t.s.
tsdisplay((Males35_44)) 
tsdisplay((diff(Males35_44))) 

cor(Males35_44,Obs)

# training and test t.s.
training_M35_44 <- Training_ds$Males35_44
test_M35_44 <- test_ds$Males35_44


# benchmark forecast - females 0 to 14 with low counts distrn
training_M35_44 %>% croston(h=30) -> benchmark


# covariants with months

pacf(training_M25_34,lag=50)
acf(diff(training_M25_34),lag=100)

# no covariant
fit_pois1 <- tsglm(training_M35_44, model = list(past_obs = 1, past_mean = 10),
                   distr = "poisson")

fit_pois2 <- tsglm(training_M35_44, model = list(past_obs = 1, past_mean = 11),
                   distr = "poisson")# best Mod 


train_mod1 <- predict(fit_pois1, n.ahead = 30, level = 0.9)

train_mod2 <- predict(fit_pois2, n.ahead = 30, level = 0.9)

Males35_44_ts <- ts(training_M35_44,start=c(2006,05),frequency=12)


# automatted forecasts

auto1 <- forecast(Males35_44_ts,h=30)
auto_mod1 <- auto.arima(Males35_44_ts,xreg=trend_training)

auto2 <- forecast(auto_mod1,h=30,xreg=trend_test)


# check mod accuracy
accuracy(train_mod1$median,test_M35_44)# mod1
accuracy(train_mod2$median,test_M35_44)# mod2
accuracy(auto1$mean,test_M35_44)# mod2
accuracy(auto2$mean,test_M35_44)# mod2
accuracy(benchmark$mean,test_M35_44)# benchmark

# project counts from 2018 - to Dec 2025

# forecast MAkes 35-44 to Dec 2025
auto_mod1 <- auto.arima(Males35_44_ts,xreg=Obs)

auto2 <- forecast(auto_mod1,h=98,xreg=151:248)

# plot forecast
plot(auto2)

# check forecast residuals
checkresiduals(auto2)

# forecast d.s
forecast_Male35_44 <- data.frame(Male35_44 = auto2$mean)


#-- Males 45-54 years of age --#

# display t.s.
tsdisplay((Males45_54)) 
tsdisplay((diff(Males45_54))) 

# correlations between males renal count and time
cor(Males45_54,Obs)

# training and test t.s.
training_M45_54 <- Training_ds$Males45_54
test_M45_54 <- test_ds$Males45_54


# benchmark forecast - females 0 to 14 with low counts distrn
training_M45_54 %>% croston(h=30) -> benchmark


# covariants with months

pacf(training_M45_54,lag=50)
acf(diff(training_M45_54),lag=100)

# no covariant
fit_pois1 <- tsglm(training_M45_54, model = list(past_obs = 1, past_mean = 6),
                   distr = "poisson")

fit_pois2 <- tsglm(training_M45_54, model = list(past_obs = 1, past_mean = 11),
                   distr = "poisson")# best Mod 


train_mod1 <- predict(fit_pois1, n.ahead = 30, level = 0.9)

train_mod2 <- predict(fit_pois2, n.ahead = 30, level = 0.9)

Males45_54_ts <- ts(training_M45_54,start=c(2006,05),frequency=12)

trend = (1:120)/120 

# automatted forecasts

auto1 <- forecast(Males45_54_ts,h=30)
auto_mod1 <- auto.arima(Males45_54_ts,xreg=trend_training)

auto2 <- forecast(auto_mod1,h=30,xreg=trend_test)


# check mod accuracy
accuracy(train_mod1$median,test_M45_54)# mod1
accuracy(train_mod2$median,test_M45_54)# mod2
accuracy(auto1$mean,test_M45_54)# mod2
accuracy(auto2$mean,test_M45_54)# mod2
accuracy(benchmark$mean,test_M45_54)# benchmark

# project counts from 2018 - to Dec 2025

# forecast MAkes 45 to 54 Dec 2025
Males45_54 %>% croston(h=98) -> benchmark

plot(benchmark)

# forecast d.s
forecast_Male45_54 <- data.frame(Male45_54 = benchmark$mean)


#-- Males 55-64 years of age --#

# display t.s.
tsdisplay((Males55_64)) 
tsdisplay((diff(Males55_64))) 

cor(Males55_64,Obs)

# training and test t.s.
training_M55_64 <- Training_ds$Males55_64
test_M55_64 <- test_ds$Males55_64


# benchmark forecast - females 0 to 14 with low counts distrn
training_M55_64 %>% croston(h=30) -> benchmark


# covariants with months

pacf(training_M55_64,lag=50)
acf(diff(training_M55_64),lag=100)

# no covariant
fit_pois1 <- tsglm(training_M55_64, model = list(past_obs = 1, past_mean = 12),
                   distr = "poisson")

fit_pois2 <- tsglm(training_M55_64, model = list(past_obs = 1, past_mean = 11),
                   distr = "poisson")# best Mod 


train_mod1 <- predict(fit_pois1, n.ahead = 30, level = 0.9)

train_mod2 <- predict(fit_pois2, n.ahead = 30, level = 0.9)

Males55_64_ts <- ts(training_M55_64,start=c(2006,05),frequency=12)


# automatted forecasts

auto1 <- forecast(Males55_64_ts,h=30)
auto_mod1 <- auto.arima(Males55_64_ts,xreg=trend_training)

auto2 <- forecast(auto_mod1,h=30,xreg=trend_test)


# check mod accuracy
accuracy(train_mod1$median,test_M55_64)# mod1
accuracy(train_mod2$median,test_M55_64)# mod2
accuracy(auto1$mean,test_M55_64)# mod1
accuracy(auto2$mean,test_M55_64)# mod2
accuracy(benchmark$mean,test_M55_64)# benchmark

# project counts from 2018 - to Dec 2025

Males55_64_ts <- ts(Males55_64,start=c(2006,05),frequency=12)


# automatted forecasts
auto_mod1 <- auto.arima(Males55_64_ts,xreg=Obs)

auto2 <- forecast(auto_mod1,h=98,xreg=151:248)


plot(auto2)

checkresiduals(auto2)

# forecast d.s
forecast_Male55_64 <- data.frame(Male55_64 = auto2$mean)



#-- Males 65-75 years of age --#

# display t.s.
tsdisplay((Males65_74)) 
tsdisplay((diff(Males65_74))) 

cor(Males65_74,Obs)

# training and test t.s.
training_M65_74 <- Training_ds$Males65_74
test_M65_74 <- test_ds$Males65_74


# benchmark forecast - females 0 to 14 with low counts distrn
training_M65_74 %>% croston(h=30) -> benchmark


# covariants with months

pacf(training_M65_74,lag=50)
acf(diff(training_M65_74),lag=100)

# no covariant
fit_pois1 <- tsglm(training_M65_74, model = list(past_obs = 1, past_mean = 12),
                   distr = "poisson")

fit_pois2 <- tsglm(training_M65_74, model = list(past_obs = 1, past_mean = 11),
                   distr = "poisson")# best Mod 


train_mod1 <- predict(fit_pois1, n.ahead = 30, level = 0.9)

train_mod2 <- predict(fit_pois2, n.ahead = 30, level = 0.9)

Males65_74_ts <- ts(training_M65_74,start=c(2006,05),frequency=12)


# automatted forecasts

auto1 <- forecast(Males65_74_ts,h=30)
auto_mod1 <- auto.arima(Males65_74_ts,xreg=trend_training)

auto2 <- forecast(auto_mod1,h=30,xreg=trend_test)


# check mod accuracy
accuracy(train_mod1$median,test_M65_74)# mod1
accuracy(train_mod2$median,test_M65_74)# mod2
accuracy(auto1$mean,test_M65_74)# mod1
accuracy(auto2$mean,test_M65_74)# mod2
accuracy(benchmark$mean,test_M65_74)# benchmark

# project counts from 2018 - to Dec 2025

Males65_74_ts <- ts(Males65_74,start=c(2006,05),frequency=12)


# automatted forecasts

auto_mod1 <- auto.arima(Males65_74_ts,xreg=Obs)

auto2 <- forecast(auto_mod1,h=98,xreg=151:248)

plot(auto2)

checkresiduals(auto2)

# forecast d.s
forecast_Male65_74 <- data.frame(Male65_74 = auto2$mean)


#-- Males 75-84 years of age --#

# display t.s.
tsdisplay((Males75_84)) 
tsdisplay((diff(Males75_84))) 

cor(Males75_84,Obs)

# training and test t.s.
training_M75_84 <- Training_ds$Males75_84
test_M75_84 <- test_ds$Males75_84


# benchmark forecast - females 0 to 14 with low counts distrn
training_M75_84 %>% croston(h=30) -> benchmark


# covariants with months

pacf(training_M75_84,lag=50)
acf(diff(training_M75_84),lag=100)

# no covariant
fit_pois1 <- tsglm(training_M75_84, model = list(past_obs = 1, past_mean = 4),
                   distr = "poisson")

fit_pois2 <- tsglm(training_M75_84, model = list(past_obs = 1, past_mean = 13),
                   distr = "poisson")# best Mod 


train_mod1 <- predict(fit_pois1, n.ahead = 30, level = 0.9)

train_mod2 <- predict(fit_pois2, n.ahead = 30, level = 0.9)

Males75_84_ts <- ts(training_M75_84,start=c(2006,05),frequency=12)


# automatted forecasts

auto1 <- forecast(Males75_84_ts,h=30)
auto_mod1 <- auto.arima(Males75_84_ts,xreg=trend_training) # BEST MOD

auto2 <- forecast(auto_mod1,h=30,xreg=trend_test)


# check mod accuracy
accuracy(train_mod1$median,test_M75_84)# mod1
accuracy(train_mod2$median,test_M75_84)# mod2
accuracy(auto1$mean,test_M75_84)# mod1
accuracy(auto2$mean,test_M75_84)# mod2
accuracy(benchmark$mean,test_M75_84)# benchmark

# project counts from 2018 - to Dec 2025

Males75_84_ts <- ts(Males75_84,start=c(2006,05),frequency=12)


# automatted forecasts - BEST FORECAST
auto_mod1 <- auto.arima(Males75_84_ts,xreg=Obs)

auto2 <- forecast(auto_mod1,h=98,xreg=151:248)


plot(auto2)

checkresiduals(auto2)

# forecast d.s
forecast_Male75_84 <- data.frame(Male75_84 = auto2$mean)


#-- Males 85+ years of age --#

# display t.s.
tsdisplay((Males85.)) 
tsdisplay((diff(Males85.))) 

cor(Males85.,Obs)

# training and test t.s.
training_M85 <- Training_ds$Males85.
test_M85 <- test_ds$Males85.


# benchmark forecast - females 0 to 14 with low counts distrn
training_M85  %>% croston(h=30) -> benchmark


# covariants with months

pacf(training_M85,lag=50)
acf(diff(training_M85),lag=100)

# no covariant
fit_pois1 <- tsglm(training_M85, model = list(past_obs = 1, past_mean = 3),
                   distr = "poisson")

fit_pois2 <- tsglm(training_M85, model = list(past_obs = 1, past_mean = 12),
                   distr = "poisson") 


train_mod1 <- predict(fit_pois1, n.ahead = 30, level = 0.9)

train_mod2 <- predict(fit_pois2, n.ahead = 30, level = 0.9)

Males85_ts <- ts(training_M85,start=c(2006,05),frequency=12)


# automatted forecasts

auto1 <- forecast(Males85_ts,h=30)
auto_mod1 <- auto.arima(Males85_ts,xreg=trend_training)

auto2 <- forecast(auto_mod1,h=30,xreg=trend_test)


# check mod accuracy
accuracy(train_mod1$median,test_M85)# mod1
accuracy(train_mod2$median,test_M85)# mod2
accuracy(auto1$mean,test_M85)# mod1
accuracy(auto2$mean,test_M85)# mod2
accuracy(benchmark$mean,test_M85)# benchmark

# project counts from 2018 - to Dec 2025

Males85_ts <- ts(Males85.,start=c(2006,05),frequency=12)


# automatted forecasts - BEST FORECAST
auto_mod1 <- auto.arima(Males85.,xreg=Obs)

auto2 <- forecast(auto_mod1,h=98,xreg=151:248)


plot(auto2)

checkresiduals(auto2)

# forecast d.s
forecast_Male85 <- data.frame(Male85 = auto2$mean)


Male_Projection <- cbind(forecast_Male0_14,forecast_Male15_24,forecast_Male25_34,
                         forecast_Male35_44,forecast_Male45_54,forecast_Male55_64,
                         forecast_Male65_74,forecast_Male75_84,forecast_Male85)

write.csv(Male_Projection,"Male_Projection.csv")








