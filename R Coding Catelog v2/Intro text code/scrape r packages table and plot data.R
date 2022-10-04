library(rvest) 
library(magrittr)
library(tidyverse)
library(plotly)
library(lubridate)
library(fpp2)
library(MASS)


table <- read_html("https://cran.r-project.org/web/packages/available_packages_by_date.html")
 
# find the number of packages published on the Cran website

table %>% 
  html_node("table") %>%
  html_table() %>%
   mutate(Date = as.Date(Date)) %>%
   mutate(count = rev(1:nrow(.)))%>% #create row with no. obs
    as_tibble() %>% # coerce dataframe into tibble
    pkgs_v2 %>% 
     mutate(Month = format(Date,"%y%m"))%>%
     mutate(Year = format(Date,"%Y"))%>% 
     mutate(Year = as.factor(Year)) -> str(pkgs_v3)
  
  #line plot of cumulative number of packages being released on the cran website
     plot_ly(pkgs_v3,x=~Date, y=~count, name="Published packages")
  
  
  pkgs_v3 %>% # create an aggregated view by Month (cusum)
    mutate (counta = 1) %>%   
    group_by(Month) %>%
    summarise(total = sum(counta, na.rm = TRUE)) %>% 
    mutate(obs = row_number()) %>%
    mutate(cumtot = order_by(obs, cumsum(total)))%>% 
    filter (obs > 20) -> test
    
  
   plot(test$cumtot)
  
   #coerce tibblbe into a time series object  
   ts(test$cumtot,start=c(2009,09),frequency=12) -> ts_pkgs
   
   plot(ts_pkgs)
  
  # train <- window(ts_pkgs,end=c(2017,08))
  # h <- 12
  
   ARIMA <- forecast(auto.arima(ts_pkgs, lambda=0, biasadj=TRUE),h=h)
  
  #originals <- ARIMA[["x"]]
  #res <- ARIMA[["residuals"]]
  #mean(abs(res)) # mean abs errors 
  #plot((res))
   
   
  combo <- c(ARIMA[["x"]],ARIMA[["mean"]])
  combo <- data.frame(combo)
   
   names(ARIMA)
   
   typeof(ARIMA)
   
   plot(ARIMA,PI=FALSE)
   
   
   plot_ly(data=sum_data_Year,x=~Year,y=~total,name="No. of Published packages per year")

    
