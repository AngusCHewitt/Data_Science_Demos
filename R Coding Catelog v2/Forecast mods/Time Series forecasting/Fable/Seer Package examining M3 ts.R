# test seer package
# M3 Example

library(seer)

library(Mcomp)
data(M3) # load M# data into global env.

M3$N0001 # t.s. datasets

# subset t.s. based on freq.
mthly_m3 <- subset(M3, "monthly")
yearly_m3 <- subset(M3, "yearly")

m3y <- M3[1:2]

# list t.s. series feature of yearly M3 t.s.
library(tsfeatures)
M3yearly_features <- seer::cal_features(yearly_m3, database="M3", h=6, highfreq = FALSE)
head(M3yearly_features)

# test fcast accuracy using a variety of t.s. mods
tslist <- list(M3[[1]], M3[[2]])
accuracy_info <- fcast_accuracy(tslist=tslist, models= c("arima","ets","rw","rwd", "theta", "nn"), database ="M3", cal_MASE, h=6, length_out = 1, fcast_save = TRUE)
accuracy_info
