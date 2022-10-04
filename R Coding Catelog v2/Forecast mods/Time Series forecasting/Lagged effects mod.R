# Building in lagged effects to t.s. mod

library(forecast)

library(fpp2) # forecast datasets

data(insurance)

# facet sales and advertising
autoplot(insurance, facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Insurance advertising and quotations")

# Lagged predictors. Test 0, 1, 2 or 3 lags.
Advert <- cbind(
  AdLag0 = insurance[,"TV.advert"],
  AdLag1 = stats::lag(insurance[,"TV.advert"],-1),
  AdLag2 = stats::lag(insurance[,"TV.advert"],-2),
  AdLag3 = stats::lag(insurance[,"TV.advert"],-3)) %>%
  head(NROW(insurance))

# correlations between 
cor(insurance[4:40,1], Advert[4:40,1:4])



# Restrict data so models use same fitting period
## use AIC to determine best mods

fit1 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1],
                   stationary=TRUE) # current mth advert
fit2 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:2],
                   stationary=TRUE) # current mth @ pervious mth advert
fit3 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:3],
                   stationary=TRUE)
fit4 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:4],
                   stationary=TRUE)

# aicc for each mod
c(fit1[["aicc"]],fit2[["aicc"]],fit3[["aicc"]],fit4[["aicc"]])