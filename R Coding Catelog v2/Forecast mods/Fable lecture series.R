# fable Lecture series

## Follow EG

library("tidyverse")
library("fable")
library("tsibble")
library(fpp2)

# load dataset

# install.packages("devtools")
devtools::install_github("tidyverts/tsibbledata")

tsibbledata::ansett

# rpubs e.g. https://rpubs.com/modelthinkingbr/fable
prison <- fpp2::prisonLF %>% 
  mutate(qtr = yearquarter(t)) %>% 
  select(-t) %>% 
  as_tsibble(index = qtr, key = id(state, gender,legal))
prison

# grouped fitted ets - need to add model to handle t.s. meta data

ets_mod <- function(df) {
  
  model(ETS(log(count)))
  
}

nest_t.s <- prison %>% 
  mutate(ets_mod = map(ets_mod)) 


nest_t.s %>% mutate(fcast = map())
