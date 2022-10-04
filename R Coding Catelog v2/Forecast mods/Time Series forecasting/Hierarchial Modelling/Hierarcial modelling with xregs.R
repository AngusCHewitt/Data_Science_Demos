library(hts)
plot(infantgts)

plot(log(aggts(infantgts, level=0)), xlab="Year", ylab="Log total infant deaths")
y <- log(aggts(infantgts, level=0))
z <- pmax(time(infantgts$bts) - 1970, 0)
fit <- lm(y ~ z)
lines(ts(fitted(fit),start=1933),col='red')

y = window(infantgts, start=1944)
z = pmax(time(y$bts) - 1970, 0)
fz = max(z) + 1:10
fc = forecast(y, h=10, fmethod="arima", xreg=z, newxreg=fz, lambda=0)
plot(fc)