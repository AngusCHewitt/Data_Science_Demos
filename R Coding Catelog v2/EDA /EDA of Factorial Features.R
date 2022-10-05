##-- reshape dataset to form around the prob of occurance for partiucular player type --##
library(tidyverse)
library(vcd)


## test fit of cont vars
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

# fit binomial logistic model to examine relation between HGS and form
ggplot(data = AFL_tabs_dt, aes(y = HGS_Binary,  x=PCA_Shot_Acc_2 )) +   geom_jitter() +
  binomial_smooth()
