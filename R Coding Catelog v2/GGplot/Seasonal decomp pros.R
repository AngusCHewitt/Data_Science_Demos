
Prosthesis <- read.csv("~/Prosthesis.csv")
View(Prosthesis)
str(Prosthesis)

pros_ts <- ts(Prosthesis$Prosthesis,start = c(2014, 2),frequency=12)

plot_ts <- stl(pros_ts, s.window = "per", robust=TRUE)

plot(plot_ts,main="Seasonal Decomposition of Prosthesis - Claim Liab")

