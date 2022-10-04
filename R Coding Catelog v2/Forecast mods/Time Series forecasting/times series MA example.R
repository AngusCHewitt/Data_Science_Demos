

dat <- c(1,2,3,4,5,6,7,8,9,10)

movavg(timeSeries::lag(timeSeries(dat), k = 1),n = 3, "s")