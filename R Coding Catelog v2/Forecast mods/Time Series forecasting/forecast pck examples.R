# AUS Beer
library(forecast) # forecast functions
library(seasonal) # time series functions
library(fpp2) # data attached to online text book - Forecasting: Principles and Practise, Rob Hyndman 

# load data
data(ausbeer)

# plot t.s.
plot(ausbeer)

# examine data object attributes and class
is(ausbeer)
attributes(ausbeer)
