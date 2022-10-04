library(forecast)
library(tidyverse)

# read in data
ANZDATA <- read_excel("OneDrive - Department of Health and Human Services. Victoria/ANZDATA - SW Renal and Transplants.xlsx")

str(ANZDATA)


# training and test dataset for response variable
ts_dialysis <- ts(ANZDATA$Dialysis, start = c(1997), frequency = 1)

# resp training dataset
ts_dialysis_resp <- window(ts_dialysis, end = c(2013)) 



# training and test dataset for explanatory variable
ts_trans <- ts(ANZDATA$`Functioning Transplant`, start = c(1997), frequency = 1)

# resp training dataset
ts_transplants_expl <- window(ts_trans, end = c(2013)) 


fit <- tslm(ts_dialysis_resp ~ trend + ts_transplants_expl)

summary(fit)
plot(residuals(fit))


# resp testing data
ts_dialysis_resp_test <- window(ts_dialysis, start = c(2014)) 

# resp testing data
ts_transplants_expl_test <- data.frame(ts_transplants_expl = window(ts_trans, start = c(2014))) 


fcast <- forecast(fit, newdata = ts_transplants_expl_test, h = 4)

accuracy(ts_dialysis_resp_test, fcast$mean)

plot(fcast)

#-- future projections
forecast()


