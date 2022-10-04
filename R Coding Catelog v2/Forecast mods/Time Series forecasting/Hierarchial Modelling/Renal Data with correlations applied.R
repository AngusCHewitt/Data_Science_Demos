# Hierarcial models agg yearly data

#-- modelling t.s. variables with high correlations with time --#

library(forecast)
library(fpp2)
library(hts)
library(tidyverse)

Dataset <- read.csv("Renal Registry correl - Age Sex.csv")

str(Dataset)

str(Dataset)
ncol(Dataset)

# t.s. series dataset
ts_data <-  ts(Dataset[3:12],start=c(2005),end=c(2017),frequency = 1)

#training dataset
training_ds <- window(ts_data,end=c(2014))

# test dataset for each obs
test_ds <- window(ts_data,start=c(2015))

# character arg used to determine the number of bottom line nodes
## need to input and equal number of characters in the dimnames 
renal_training.hts <- hts(training_ds, characters = c(1,5))

# create hierarachial mods
mod_h1 <- forecast(renal_training.hts , method="bu", fmethod="arima",keep.resid=TRUE,h=3,xreg=Dataset$Years[1:10],newxreg=Dataset$Years[11:13])
mod_h2 <- forecast(renal_training.hts , method="comb", fmethod="arima",keep.resid=TRUE,h=3,xreg=Dataset$Years[1:10],newxreg=Dataset$Years[11:13])

# create hierarachial mods
mod_h3 <- forecast(renal_training.hts , method="comb", fmethod="arima",keep.resid=TRUE,h=3)
mod_h4 <- forecast(renal_training.hts , method="bu", fmethod="arima",keep.resid=TRUE,h=3)

# test forecasy accuracys
accuracy(mod_h1$bts, test_ds)
accuracy(mod_h2$bts, test_ds)
accuracy(mod_h3$bts, test_ds)
accuracy(mod_h4$bts, test_ds)


plot(mod_h1)
plot(mod_h2)

write.csv(mod_h1$bts,"arima_cor.csv")


#-- modelling hierarcial t.s. --#


str(Dataset)

str(Dataset)
ncol(Dataset)

# create t.s. dataset
ts_data <-  ts(Dataset[3:12],start=c(2005),end=c(2017),frequency =1)


#training dataset
training_ds <- window(ts_data,end=c(2014))

# test dataset for each obs
test_ds <- window(ts_data,start=c(2015))


# character arg used to determine the number of bottom line nodes
## need to input and equal number of characters in the dimnames 
renal_training.gts <- gts(training_ds, characters = c(1,5), gnames = c("Sex","Sex*Age"))

# bu ARMIA forecast using group str 
mod_g1 <- forecast(renal_training.gts , method="bu", fmethod="arima",xreg=Dataset$Years[1:10],newxreg=Dataset$Years[11:13])

# comp ARIMA forecast using group str # best mod
mod_g2 <- forecast(renal_training.gts , method="comb", fmethod="arima",xreg=Dataset$Years[1:10],newxreg=Dataset$Years[11:13])

# bu ETS forecast using group str
mod_g3 <- forecast(renal_training.gts , method="bu", fmethod="rw",drift=TRUE,xreg=Dataset$Years[1:10],newxreg=Dataset$Years[11:13])

# comp ETS using group str
mod_g4 <- forecast(renal_training.gts , method="comb", fmethod="rw",drift=TRUE,xreg=Dataset$Years[1:10],newxreg=Dataset$Years[11:13])

# bu ETS forecast using group str
mod_g5 <- forecast(renal_training.gts , method="bu", fmethod="ets")

# comp ETS using group str
mod_g6 <- forecast(renal_training.gts , method="comb", fmethod="ets")

# review forecast errors (test t.s. - training t.s. mod)
write.csv(mod_g2$bts,"gts_Arima_comb.csv")


# create list for all mod outputs
list_mod <- ts(mod_g)

plot(mod_g1) 
plot(mod_g2) # best mod
plot(mod_g3) 
plot(mod_g4) 
plot(mod_g5) 
plot(mod_g6) 

   
# test forecasy accuracy
accuracy(mod_g1$bts, test_ds)
accuracy(mod_g2$bts, test_ds) # best mod
accuracy(mod_g3$bts, test_ds)
accuracy(mod_g4$bts, test_ds)
accuracy(mod_g5$bts, test_ds)
accuracy(mod_g6$bts, test_ds)
