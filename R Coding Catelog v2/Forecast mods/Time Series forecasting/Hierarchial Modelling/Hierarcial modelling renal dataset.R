# Modelling No. Renal Diaylsis using hierarical models (plan A)

# bottom up model which forecasts each bottom level group seperately

library(forecast)
library(fpp2)
library(hts)
library(tidyverse)

# load dataset
Dataset <- read.csv("Hierarcial Dataset.csv")

str(Dataset)

Dataset$obs <- 1:150

Dataset %>%


# create t.s. dataset
ts_data <-  ts(Dataset[3:38],start=c(2006,5),end=c(2018,10),frequency =12)


#training dataset
training_ds <- window(ts_data,end=c(2016,10))

# test dataset for each obs
test_ds <- window(ts_data,start=c(2016,11))

# totals test dataset
totals <- ts(Dataset$Row_totals[127:150],start=c(2016,11),frequency =12)


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

# review forecast errors (test t.s. - training t.s. mod)
write.csv(mod_g2$bts,"Arima2.csv")


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


