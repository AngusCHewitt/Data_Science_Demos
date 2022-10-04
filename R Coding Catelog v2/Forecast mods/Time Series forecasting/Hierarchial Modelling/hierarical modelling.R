library(forecast)
library(fpp2)
library(hts)

#-- grouped modelling --#

# example of forecasting prison grouped t.s.

summary(prison)

#length of forecast and test dataset
h=8

#training dataset
training_ds <- window(prison,end=c(2014,4))

test_ds <- window(prison,start=c(2015,1))

# 3 grouping factors * 1 total * 9 nodes
prison.gts <- gts(training_ds/1e3, characters = c(3,1,9),
  gnames = c("State", "Gender", "Legal",
             "State*Gender", "State*Legal",
             "State*Gender*Legal"))
             
 
 mod_g <- forecast(prison.gts, method="
 ", fmethod="ets")
 
   
 # test forecasy accuracys
 accuracy(mod$bts*1000, test_ds)
 
 #-- topdown approach requires hierarchial t.s. --#
 
 #training dataset
training_ds <- window(visnights,end=c(2014,4))

test_ds <- window(visnights,start=c(2015,1))
 
 # example of forecast tourist number per state
 tourism.hts <- hts(training_ds, characters = c(3, 5))

# create hierarachial mods
mod_h <- forecast(tourism.hts, method="tdfp", fmethod="ets")
 
 # test forecasy accuracys
 accuracy(mod$bts, test_ds)
 
 checkresiduals(mod_h)

# plot layered forecast
plot(mod_h)

# create a kist of all mod objects
x <- ts(mod_h)

# return to tourist dollar for qtr 1 (row 1) - forecast totals 
sum(mod_h$bts[1,])

