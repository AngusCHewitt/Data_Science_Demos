
#create a time series a var in the above dataset
Weekly_ts <- ts(Dataset$ANAESTHETIST,start = c(2012, 12),frequency=12)

#plot decomposition of ts
plot(stl(Weekly_ts, "per")),main="Decompostion of Weekly Claims Per Month")

