library(tidyverse)
library(dbplyr)
library(lubridate)
library(fst)
#Source the file that contains all the login details for the CDL/prem data access.  
#source("R/database_login_details.R")
#login to the CDL
library(DBI)
this_month <- floor_date(today(),"month")


av_catchment <- read_csv("data/av_catchment.csv") %>% 
  mutate(locality = tolower(locality)) %>% 
  left_join(read_csv("data/av_catchment_campus_code.csv"))


summary(av_catchment)

con <- login_cdl()

#------------------------------------------------------------------------------------------------------

#Creates a dataset with one line per request from the public that resulted in an ambulance callout 
#Filters it to only "emergency" requests

#Requests for AV service
av_callouts_all <- tbl(con,in_schema('vhirsvads_vw','ODS_Request')) %>% 
  union_all(tbl(con,in_schema('vhirsvadshistory_vw','History_Request')))  %>% 
  filter(Incident_Type == "EMG",
         Case_Date_DT > as.Date("2015-01-01"),
         !is.na(Der_Response_Time_Case),
         #Der_Response_Time_Case< 120*60, # response times greater than 2 hours excluded
         #Team_TIG_inscope_flag == "Y" SHOULD BE INLCUDED BUT CAN"T FIND VAR IN THE CDL? 
  ) %>% 
  select(Case_Date_DT,
         Req_Locality,
         Priority_Final,
         Der_Response_Time_Case,
         Der_Activation_Time,
         Request_DT,
         Case_Number) %>% 
  collect() %>% 
  mutate(locality = tolower(Req_Locality)) %>% 
  mutate(financial_year = fy::date2fy(Case_Date_DT),
         quarter = quarter(Case_Date_DT),
         start_of_month = floor_date(Case_Date_DT, "month")) %>% 
  left_join(av_catchment) %>%
  mutate(av_pickup_location_metro_regional = if_else(is.na(hospital_abb),"regional","metro"),
         Der_Response_Time_Case = if_else(Der_Response_Time_Case<0,
                                          NA_real_,
                                          as.double(Der_Response_Time_Case)),
         invalid_repsonse_time_flag = if_else(Der_Response_Time_Case > 900,1,0)) %>% #60 minutes
  filter(Priority_Final == '1') %>%
  select(-Req_Locality,
         -hospital_abb,
         -hospital,
         -campus_code,
         -campus_name )
  
av_callouts_all_priority_3_agg <- av_callouts_all %>% 
  group_by(financial_year, quarter) %>%
  summarise(Avg_AV_priority_3_60min = mean(invalid_repsonse_time_flag, na.rm = TRUE))
            