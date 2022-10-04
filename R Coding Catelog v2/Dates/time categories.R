x <- as.POSIXct("2016-01-01 00:00:00", tz="UTC") + as.difftime(60*(0:47),units="mins")
cut(hm(hour(x),mintue(x))), breaks="2 hours", labels=FALSE)
# or to show more clearly the results:
data.frame(x, cuts = cut(x, breaks="2 hours", labels=FALSE))

cut_interval(x,10)