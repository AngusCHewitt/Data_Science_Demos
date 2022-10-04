# forecast cf's

library(tidyverse)
library(GGally)
library(RcmdrMisc)
library(Rcmdr)
library(nlme)

Dataset <- readXL("/Users/angushewitt/Desktop/Cash flow.xlsx",
                  rownames=FALSE, header=TRUE, na="", sheet="Sheet 1", 
                  stringsAsFactors=TRUE)


ts_ds <- ts(Dataset, start=c(6,1993), frequency = 4)

# ts features could help id the appropriate setting for arima
fit_mod <- arima(ts_ds[,"Cash"], order=c(1,1,0),seasonal=c(4,0,0),
      xreg=ts_ds[,c( "Observations")])

# nl t.s. 
m <- ?gls(Cash ~ Observations,
         data=ts_ds,
         correlation=corARMA(p=4, q=1))
summary(m)

data(polio)

plot(polio)

trend=(1:168/168)
cos12=cos((2*pi*(1:168))/12)
sin12=sin((2*pi*(1:168))/12)
cos6=cos((2*pi*(1:168))/6)
sin6=sin((2*pi*(1:168))/6)

plot(1:168,cos12, type = "o")

#Autoregressive Conditional Poisson Model with explaning covariates
polio_data<-data.frame(polio, trend , cos12, sin12, cos6, sin6)
mod1 <- acp(polio ~ -1 + trend + cos12 + sin12 + cos6 + sin6, data=polio_data, p = 1 ,q = 2)
summary(mod1)


#Poisson Model with explaning covariates
polio_data<-data.frame(polio, trend , cos12, sin12, cos6, sin6)
mod3 <- acp(polio ~ trend + cos12 + sin12 + cos6 + sin6, data= polio_data, family="poisson")
summary(mod3)

train<-data.frame(polio_data[c(1: 119),])
mod1t <- acp(polio~-1+trend+cos12+sin12+cos6+sin6,data=train, p = 1  ,q = 2)
xpolio_data<-data.frame(trend , cos12, sin12, cos6, sin6)
test<-xpolio_data[c(120:nrow(xpolio_data)),]
yfor<-polio_data[120:nrow(polio_data),1]

?acp::predict.acp()

predict_mod <- predict(mod1t,yfor,test)
library(acp)

plot(predict_mod - yfor)

mod1$residuals)