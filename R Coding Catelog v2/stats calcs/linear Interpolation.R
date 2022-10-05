# linear interpolation



x <- 1:10
y <- rnorm(10)
par(mfrow = c(2,1))
plot(x, y, main = "approx(.) and approxfun(.)")
points(approx(x, y), col = 2, pch = "*")
points(approx(x, y, method = "constant"), col = 4, pch = "*")


vic_pop <- read.csv("VIC Pop.csv")

str(vic_pop)

pmap(vic_pop,approx(X, Y))

x <- 1:16
y <- c(9,11)

approx(x = y,n=11)

y <- c(6321648,6892302.55828296)

approx(x = y, n = 48)


##-- linear interpolation - converting yearly t.s. into mthly t.s.
linear_interpolation <- function(df)
{
  data_year = list()
  
  
  for (i in 1:40){
    year <- approx(x = c(df$pop[i], df$pop[i + 1]), n=13)$y
    data_year[[i]] <- year[1:12]}
  data_unlisted <- unlist(data_year)
  data_unlisted <- list(data_unlisted,df$pop[41])
  data_unlisted <- unlist(data_unlisted)
  ts(data_unlisted, start = c(1991,06),frequency = 12)}


