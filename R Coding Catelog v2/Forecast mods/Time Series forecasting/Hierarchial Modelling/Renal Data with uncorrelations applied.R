# Hierarcial models agg yearly data

#-- modelling t.s. variables with high uncorrelations with time --#

library(forecast)
library(fpp2)
library(hts)
library(tidyverse)

Dataset <- read.csv("Forecasts and uncorrelations data.csv")

str(Dataset)

str(Dataset)
ncol(Dataset)

acf(diff(Dataset$F45_54)) 

# t.s. series dataset
ts_data <-  ts(Dataset[2:9],start=c(2005),end=c(2017),frequency = 1)

#training dataset
training_ds <- window(ts_data,end=c(2014))

# test dataset for each obs
test_ds <- window(ts_data,start=c(2015))

# character arg used to determine the number of bottom line nodes
## need to input and equal number of characters in the dimnames 
renal_training.gts <- gts(training_ds, characters = c(1,5), gnames = c("Sex","Sex*Age"))

# bu ARMIA forecast using group str 
mod_g1 <- forecast(renal_training.gts , method="bu", fmethod="arima")

# comp ARIMA forecast using group str
mod_g2 <- forecast(renal_training.gts , method="comb", fmethod="arima")

# bu ETS forecast using group str
mod_g3 <- forecast(renal_training.gts , method="bu", fmethod="rw",drift=TRUE)

# comp ETS using group str
mod_g4 <- forecast(renal_training.gts , method="comb", fmethod="rw",drift=TRUE)

# bu ETS forecast using group str
mod_g5 <- forecast(renal_training.gts , method="bu", fmethod="ets")

# comp ETS using group str
mod_g6 <- forecast(renal_training.gts , method="comb", fmethod="ets")

plot(mod_g1) 
plot(mod_g2) 
plot(mod_g3) 
plot(mod_g4) 
plot(mod_g5) 
plot(mod_g6) 

   
# test forecasy accuracy
accuracy(mod_g1$bts, test_ds)
accuracy(mod_g2$bts, test_ds)
accuracy(mod_g3$bts, test_ds)
accuracy(mod_g4$bts, test_ds)
accuracy(mod_g5$bts, test_ds)
accuracy(mod_g6$bts, test_ds)

# review forecast errors (test t.s. - training t.s. mod)
write.csv(mod_g2$bts,"Arima2.csv")


# create list for all mod outputs
list_mod <- ts(mod_g)


# character arg used to determine the number of bottom line nodes
## need to input and equal number of characters in the dimnames 
renal_training.hts <- hts(training_ds, characters = c(1,5))

# create hierarachial mods
mod_h1 <- forecast(renal_training.hts , method="bu", fmethod="arima")
mod_h2 <- forecast(renal_training.hts , method="comb", fmethod="arima")

# create hierarachial mods
mod_h3 <- forecast(renal_training.hts , method="comb", fmethod="ets",keep.resid=TRUE,h=3)
mod_h4 <- forecast(renal_training.hts , method="bu", fmethod="ets",keep.resid=TRUE,h=3)

# test forecasy accuracys
accuracy(mod_h1$bts, test_ds)
accuracy(mod_h2$bts, test_ds)
accuracy(mod_h3$bts, test_ds)
accuracy(mod_h4$bts, test_ds)


plot(mod_h1)
plot(mod_h2)

write.csv(mod_h1$bts,"arima_cor.csv")
