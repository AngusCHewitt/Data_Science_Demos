# need to install below add on package to run this program
## > install.packages(c("tidyverse","vcd","qichart"),dependencies=TRUE)

library(tidyverse)
library(lubridate)
library(ggplot2)
library(vcd)
library(qicharts)

Dataset <- read_csv("aids.csv")#read in csv file as a tibble

str(Dataset)

par(1,1)

#all purchases > $200 which need to sourced via an approved provider

Dataset %>%
 filter(total_greater_200 == "Y") %>%
    mutate (total_greater_200 = as.factor(total_greater_200),
     Approved_Provider =  as.factor(Approved_Provider), 
     SERVICE_ITEM_CD = as.factor(SERVICE_ITEM_CD),
     SERVICE_DT = dmy(SERVICE_DT),
     DRAWN_DT  = dmy(DRAWN_DT ),
     Service_year  = as.factor(Service_year),
     Service_Month = as.factor(Service_Month),
     Payment_Year  = as.factor(Payment_Year),
     Payment_mnth  = as.factor(Payment_mnth),
     REFERRAL_SERVICE_PROVIDER_NB  = as.factor(REFERRAL_SERVICE_PROVIDER_NB),
     Itemcode_Description = ifelse(SERVICE_ITEM_CD %in% "AD010","Miscellaneous","Other")) -> All

#mosaic plot testing assocation between itemcd and purchases made
#with approved proviedrs

mosaic(~Itemcode_Description+Approved_Provider,data=All,shade=TRUE,labeling=labeling_values,legend=FALSE,
rot_labels=c(0,45,45,45))



#p chart used to monitor the proportion of item purchased 
## through unapproved provider

All %>%
mutate(WEEKS_SERVICE_DT = ceiling_date(SERVICE_DT,"week")) %>%
group_by(WEEKS_SERVICE_DT) %>%
summarise(defect = sum(Approved_Provider == "N"),total = length(Approved_Provider)) %>%
mutate(Unapproved_prop = defect/total) -> qichart_Data

cchart <- qic(defect ,
    n        = total,
    x        = WEEKS_SERVICE_DT,
    data     = qichart_Data,
    chart    = 'p',
    multiply = 100,
    main     = 'Proportion of items purchased through unapproved providers (P chart)',
    ylab     = 'Percentage',
    xlab     = 'Weeks')






