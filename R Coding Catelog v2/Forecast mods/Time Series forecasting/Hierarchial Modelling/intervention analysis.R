library(forecast)
library(tidyverse)
library(GGally)

#-- intervention analysis --#
Dataset <- readXL("//internal.vic.gov.au/DHHS/HomeDirs6/ahew2812/Documents/Intervention analysis of colonoscopies ds.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="Sheet1", stringsAsFactors=TRUE)


# visualise t.s. of vanilla colonoscopies facted by h.s. 
p <- ggplot(data = Dataset, aes(x = Date, y = vanilla_colons))

p + geom_point() + geom_line() + facet_wrap(~ net_name)


str(Dataset)

# spread d.s. by health services
Dataset %>%
  select(Date, vanilla_colons, net_name) %>% 
  spread(net_name,vanilla_colons) -> tidy_ds

ggpairs(tidy_ds)
  

# t.s. dataset for tidy data
ts_dataset <- ts(tidy_ds[2:15], start = c(2012,7), frequency = 12)

# test data
test_dataset <- ts(tidy_ds[2], start = c(2012,7), frequency = 12)


plot(test_dataset)

plot(diff(test_dataset))


# training dataset
test_window <- window(test_dataset,end=c(2017,12))

test_ds <- window(test_dataset,end=c(2017,1))

# fcast d.s.
auto_fcast <- forecast(test_ds, h = 11)

checkresiduals(auto_fcast)
accuracy(auto_fcast,test_window )


# plot actuals and projections
autoplot(test_window) + autolayer( auto_fcast$mean,series="stlf_auto") 


# intervention analysis with tscount

library("tscount")

interventions <- interv_covariate(n = length(test_dataset), tau = c(67, 82),
                                  delta = c(1, 0))
campyfit_pois <- tsglm(test_dataset, model = list(past_obs = 1, past_mean = 12),
                       xreg = interventions, distr = "poisson")
campyfit_nbin <- tsglm(test_dataset, model = list(past_obs = 1, past_mean = 12),
                       xreg = interventions, distr = "nbinom")


checkresiduals(campyfit_pois)


par(mfrow = c(2, 2), mar=c(3, 4, 2, 0.5), mgp=c(1.8, 0.6, 0))
acf(residuals(campyfit_pois), main="", xlab="Lag (in years)")
title(main = "ACF of response residuals")
marcal(campyfit_pois, main = "Marginal calibration")
lines(marcal(campyfit_nbin, plot = FALSE), lty = "dashed")
legend("bottomright", legend = c("Pois", "NegBin"), lwd=1,
       lty=c("solid", "dashed"))
pit(campyfit_pois, ylim = c(0, 1.5), main = "PIT Poisson")
pit(campyfit_nbin, ylim = c(0, 1.5), main = "PIT Negative Binomial")

