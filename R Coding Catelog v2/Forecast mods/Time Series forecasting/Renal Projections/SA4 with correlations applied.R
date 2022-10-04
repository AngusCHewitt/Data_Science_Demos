#-- SA4 models from ANZDATA --#

#-- modelling hierarcial t.s. --#

library(forecast)
library(fpp2)
library(hts)
library(tidyverse)


# load file
Dataset <- read.csv("SA4 ANZDATA.csv")


str(Dataset)

str(Dataset)
ncol(Dataset)

# create t.s. dataset
ts_data <-  ts(Dataset[2:18],start=c(2005),end=c(2017),frequency =1)


#training dataset
training_ds <- window(ts_data,end=c(2014))

# test dataset for each obs
test_ds <- window(ts_data,start=c(2015))


# character arg used to determine the number of bottom line nodes
## need to input and equal number of characters in the dimnames 
renal_training.gts <- gts(training_ds, characters = c(6,4), gnames = c("Location","Location*Rate"))

# bu ARMIA forecast using group str 
mod_g1 <- forecast(renal_training.gts , method="bu", fmethod="arima",xreg=Dataset$Year[1:10],newxreg=Dataset$Year[11:13],h=3)

# comp ARIMA forecast using group str mod_g2 <- forecast(renal_training.gts , method="comb", fmethod="arima",xreg=Dataset$Year[1:10],newxreg=Dataset$Year[11:13],h=3)

# bu ETS forecast using group str
mod_g3 <- forecast(renal_training.gts , method="bu", fmethod="rw",drift=TRUE,xreg=Dataset$Years[1:10],newxreg=Dataset$Year[11:13],h=3)

# comp ETS using group str
mod_g4 <- forecast(renal_training.gts , method="comb", fmethod="rw",drift=TRUE,xreg=Dataset$Years[1:10],newxreg=Dataset$Year[11:13],h=3)

# bu ETS forecast using group str
mod_g5 <- forecast(renal_training.gts , method="bu", fmethod="ets",h=3)

# comp ETS using group str
mod_g6 <- forecast(renal_training.gts , method="comb", fmethod="ets",h=3)

# review forecast errors (test t.s. - training t.s. mod)
write.csv(mod_g2$bts,"gts_Arima_comb.csv")


# create list for all mod outputs
list_mod <- ts(mod_g)

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

write.csv(mod_g5$bts,"ets_diffs.csv")
