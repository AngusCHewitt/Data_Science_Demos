##-- visulase the density fun al all number vars 
densityPlot_fun <- function(df) {

require(tidyverse)  

## geom density
df %>% 
  select_if(is.numeric) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")}