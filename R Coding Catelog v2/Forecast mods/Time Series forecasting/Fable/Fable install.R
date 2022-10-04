# install.packages("devtools")

devtools::install_github("tidyverts/fable")

library(fable)
library(tsibbledata)
UKLungDeaths %>%
  model(ets = ETS(log(mdeaths))) %>%
  forecast