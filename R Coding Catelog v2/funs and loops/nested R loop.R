# nest R loop

# example of nested loop

library(tidyverse)
library(lubridate)
library(RcmdrMisc)


Dataset <- readXL("//internal.vic.gov.au/DHHS/HomeDirs6/ahew2812/Desktop/ESIS_DATE_LIST.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="Data", stringsAsFactors=TRUE)

test_ds <- Dataset

# loop throught end date and i.d. quene jumper
for (i in 1:seq_along(by = 1009)){
  for (j in 2:seq_along(by = 1)){
    if (test_ds$ending_date[i] > test_ds$ending_date[j])
      test_ds$results[i] = "JUMPED" else {test_ds$results[i] = "NOT_JUMPING"}
}
}

summary(as.factor(test_ds$results))

write.csv(test_ds, "test_ds.csv")

Dataset %>%
  mutate(starting_date = ymd(starting_date)) %>%
  mutate(ending_date = ymd(ending_date)) -> Dataset
  

str(Dataset)

