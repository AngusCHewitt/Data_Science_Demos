# Hierarichal mod ANZDATA
# SA3 Proportions

library(forecast)
library(fpp2)
library(hts)
library(tidyverse)

# load dataset
Dataset <- read.csv("SA3 Proportions of dialysis.csv")

# struture and no. cols
str(Dataset)
ncol(Dataset)

# t.s. series dataset
ts_data <-  ts(Dataset[2:67],start=c(2005),end=c(2017),frequency = 1)


#training dataset
training_ds <- window(prop ,end=c(2014))

# test dataset for each obs
test_ds <- window(prop ,start=c(2015))

# character arg used to determine the number of bottom line nodes
## need to input and equal number of characters in the dimnames 
renal_training.hts <- hts(training_ds, characters = c(4,2))

# create hierarachial mods
mod_h1 <- forecast(renal_training.hts , method="tdgsa", fmethod="arima",keep.resid=TRUE,h=3,xreg=Dataset$Year[1:10],newxreg=Dataset$Year[11:13], lambda=0)
mod_h2 <- forecast(renal_training.hts , method="tdgsf", fmethod="arima",keep.resid=TRUE,h=3,xreg=Dataset$Year[1:10],newxreg=Dataset$Year[11:13], lambda=0)

# create hierarachial mods
mod_h3 <- forecast(renal_training.hts , method="tdgsa", fmethod="rw",keep.resid=TRUE,h=3=,drift=TRUE)
mod_h4 <- forecast(renal_training.hts , method="tdgsf", fmethod="rw",keep.resid=TRUE,h=3,drift=TRUE)

# create hierarachial mods
mod_h5 <- forecast(renal_training.hts , method="tdgsa", fmethod="ets",keep.resid=TRUE,h=3)
mod_h6 <- forecast(renal_training.hts , method="tdgsf", fmethod="ets",keep.resid=TRUE,h=3)


# test forecasy accuracys
accuracy(mod_h1$bts, test_ds)
accuracy(mod_h2$bts, test_ds)
accuracy(mod_h3$bts, test_ds)
accuracy(mod_h4$bts, test_ds)
accuracy(mod_h5$bts, test_ds)
accuracy(mod_h6$bts, test_ds)

?hts()

sum(mod_h6$bts[1,])

write.csv(mod_h6$bts/100,"SA3 Proportions.csv")


#-- Grouped Modelling --#

# character arg used to determine the number of bottom line nodes
## need to input and equal number of characters in the dimnames 
renal_training.gts <- gts(training_ds, characters = c(4,2), 
gnames = c("SA3", "Area","SA3*Area"))

# create hierarachial mods
mod_g1 <- forecast(renal_training.gts , method="bu", fmethod="arima",keep.resid=TRUE,h=3,xreg=Dataset$Year[1:10],newxreg=Dataset$Year[11:13])
mod_g2 <- forecast(renal_training.gts , method="comb", fmethod="arima",keep.resid=TRUE,h=3,xreg=Dataset$Year[1:10],newxreg=Dataset$Year[11:13])

# create hierarachial mods
mod_g3 <- forecast(renal_training.gts , method="bu", fmethod="ets",keep.resid=TRUE,h=3)
mod_g4 <- forecast(renal_training.gts , method="comb", fmethod="ets",keep.resid=TRUE,h=3)

# arima with trend
accuracy(mod_g1$bts, test_ds)
accuracy(mod_g2$bts, test_ds)


