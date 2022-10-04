# Hierarichal mod ANZDATA
# hierarical times by SA4, Age, Sex

library(forecast)
library(fpp2)
library(hts)
library(tidyverse)

Dataset <- read.csv("ANZDATA HieraricalMod.csv")

str(Dataset)
ncol(Dataset)

# t.s. series dataset
ts_data <-  ts(Dataset[2:69],start=c(2005),end=c(2017),frequency = 1)

#training dataset
training_ds <- window(ts_data,end=c(2014))

# test dataset for each obs
test_ds <- window(ts_data,start=c(2015))

# character arg used to determine the number of bottom line nodes
## need to input and equal number of characters in the dimnames 
renal_training.hts <- hts(training_ds, characters = c(6,5,1))

# create hierarachial mods
mod_h1 <- forecast(renal_training.hts , method="bu", fmethod="arima",keep.resid=TRUE,h=3,xreg=Dataset$Year[1:10],newxreg=Dataset$Year[11:13], lambda=0)
mod_h2 <- forecast(renal_training.hts , method="comb", fmethod="arima",keep.resid=TRUE,h=3,xreg=Dataset$Year[1:10],newxreg=Dataset$Year[11:13], lambda=0)

# create hierarachial mods
mod_h3 <- forecast(renal_training.hts , method="comb", fmethod="arima",keep.resid=TRUE,h=3)
mod_h4 <- forecast(renal_training.hts , method="bu", fmethod="arima",keep.resid=TRUE,h=3)



# test forecasy accuracys
accuracy(mod_h1$bts, test_ds)
accuracy(mod_h2$bts, test_ds)
accuracy(mod_h3$bts, test_ds)
accuracy(mod_h4$bts, test_ds)


plot(mod_h)

write.csv(mod_h$bts,"ets.csv")


#-- Grouped Modelling --#

# character arg used to determine the number of bottom line nodes
## need to input and equal number of characters in the dimnames 
renal_training.gts <- gts(training_ds, characters = c(6,5,1), 
gnames = c("SA3", "Age", "Sex",
"SA3*Age", "SA3*Sex",
"SA3*Age*Sex"))

# create hierarachial mods
mod_g1 <- forecast(renal_training.gts , method="bu", fmethod="ets",keep.resid=TRUE,h=3,xreg=Dataset$Year[1:10],newxreg=Dataset$Year[11:13])
mod_g2 <- forecast(renal_training.gts , method="comb", fmethod="ets",keep.resid=TRUE,h=3,xreg=Dataset$Year[1:10],newxreg=Dataset$Year[11:13])

# create hierarachial mods
mod_g3 <- forecast(renal_training.gts , method="bu", fmethod="ets",keep.resid=TRUE,h=3)
mod_g4 <- forecast(renal_training.gts , method="comb", fmethod="ets",keep.resid=TRUE,h=3)

# arima with trend
accuracy(mod_g1$bts, test_ds)
accuracy(mod_g2$bts, test_ds)

#arima
accuracy(mod_g3$bts, test_ds)
accuracy(mod_g4$bts, test_ds)



plot(mod_h)

write.csv(mod_h$bts,"ets.csv")


