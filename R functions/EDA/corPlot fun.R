
##- cor plot visual with circles density plots desc the strength of cor between pairs
corPlot_fun <- function(df) {

require(tidyverse)

##corplot using circle colour density
df %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot() 

##-- dfwith correlations pairings
df %>%
  select_if(is.numeric) %>%
  cor() -> cor_Data

cor_Data

}