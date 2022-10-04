# intervention analysis for colonoscopy funding uplift
library(tidyverse)
library(forecast)
library(zoo)

Dataset <- readXL("F:/Health Services Programs/System Design Planning & Decision Support/Decision Support/Projects/Colonoscopy reporting/Intervention Analysis/Intervention analysis of colonoscopies.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="Data", stringsAsFactors=TRUE)



str(Dataset)

# add month and year together 
Dataset$Date <- as.yearmon(paste(Dataset$year, Dataset$month), "%Y %m")


# mutate date var.


ggplot(Dataset, aes(x = Date, y = basic_colons_cnt, color = net_name)) + geom_point() + geom_line() + facet_wrap(~net_name)

summary(Dataset$Date)