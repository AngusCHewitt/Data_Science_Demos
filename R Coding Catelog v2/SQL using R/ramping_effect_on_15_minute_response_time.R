
library(tidyverse)
library(dbplyr)
library(lubridate)
#Source the file that contains all the login details for the CDL/prem data access.  
#source("R/database_login_details.R")
#login to the CDL
library(DBI)
# hi


transfer_times <- read_fst("data/transfer_times.fst")


#25 mintues. 

transfer_times_by_hospital <- 
transfer_times %>% 
  filter(known_hospital == 1,
         month == ymd("2022-04-01")) %>% 
  mutate(ramp_time_qc_flag = case_when(ramp_time_total <0 ~ F,
                                 is.na(ramp_time_total) ~ F,
                                 T~T),
         ramp_time = pmin(12*60*60,ramp_time_total) ) %>% 
  filter(ramp_time_qc_flag) %>%
  mutate(changed_ramp_time  = case_when(campus_name %in% c("Austin Hospital" ,
                                                                "Monash Medical Centre (Clayton)",
                                                                "The Alfred"  ) ~ 30,
                                             T ~ ramp_time)
         ) 

transfer_times_by_hospital%>% 
  summarise(ramp_times = mean(ramp_time)/60,
            ramp_times_new = mean(changed_ramp_time)/60) %>% 
    mutate(change = ramp_times - ramp_times_new)

transfer_times_by_hospital %>% 
  group_by(campus_name) %>% 
  summarise(number = n()) %>% 
  ungroup() %>% 
    mutate(share = number/sum(number)) %>% arrange(desc(share))

av_catchment <- read_csv("data/av_catchment.csv") %>% 
  mutate(locality = tolower(locality)) %>% 
  left_join(read_csv("data/av_catchment_campus_code.csv"))
  


#con <- dbConnect(odbc::odbc(), "CDL", timeout = 10)


#Median wait time for AV to get ready for a new patient is around 25 minutes. 


#Requests including non-cat 1 requests

callouts_all <- tbl(con,in_schema('vhirsvads_vw','ODS_Request')) %>% 
  union_all(tbl(con,in_schema('vhirsvadshistory_vw','History_Request'))) %>% 
  filter(Incident_Type == "EMG",
         Case_Date_DT > as.Date("2015-01-01"),
         !is.na(Der_Response_Time_Case),
         #Team_TIG_inscope_flag == "Y"
  ) %>% 
  select(Case_Date_DT,
         Req_Locality,
         Disp_Final,
         Der_Response_Time_Case,
         Request_DT) %>% 
  collect() %>% 
  mutate(hour = hour(Request_DT),
         hour_group = case_when(hour < 9 ~ 1,
                                hour < 17 ~ 2,
                                T ~ 3),
         locality = tolower(Req_Locality)) %>% 
  inner_join(av_catchment) %>% 
  group_by(date = Case_Date_DT,
           campus_code,
           campus_name,
           hour_group) %>% 
  summarise(av_requests = n())

callouts <- tbl(con,in_schema('vhirsvads_vw','ODS_Request')) %>% 
  union_all(tbl(con,in_schema('vhirsvadshistory_vw','History_Request'))) %>% 
  filter(Incident_Type == "EMG",
         Case_Date_DT > as.Date("2015-01-01"),
         !is.na(Der_Response_Time_Case),
        Priority_First %in% c("0","1"),
        Priority_Final %in% c("0","1"),
         #Team_TIG_inscope_flag == "Y"
         ) %>% 
  select(Case_Date_DT,
         Req_Locality,
         Disp_Final,
         Der_Response_Time_Case,
         Request_DT) %>% 
  collect() %>% 
  mutate(hour = hour(Request_DT),
         hour_group = case_when(hour < 9 ~ 1,
                                hour < 17 ~ 2,
                                T ~ 3),
         locality = tolower(Req_Locality)) %>% 
  inner_join(av_catchment) %>% 
  group_by(date = Case_Date_DT,
           campus_code,
           campus_name,
           hour_group) %>% 
  summarise(av_response_time = mean(Der_Response_Time_Case, na.rm = TRUE),
            share_less_15_mins = mean(if_else(Der_Response_Time_Case > (15*60),0,1))) %>% 
  left_join(callouts_all)




#patient_hosp_codes  
 hosp_codes <- tbl(con,in_schema('vhirsvads_vw','ODS_PatTrans')) %>% 
  filter(Trans_Flag == "Y") %>%
  select(at_scene_time = AtPat_DT,
         hosp_arr_time = AtDest_DT_PCR,
         hosp_triage_completed_time =Triage_Comp_DT,
         hosp_handover_completed_time = Handover_Comp_DT,
         ready_for_new_pat_time = Clear_DT_PCR,
         date = Case_Date_DT,
         hospital = DA_Details,
         hospital_code = DA_Loc_Cd) %>%
  group_by(hospital,hospital_code) %>% 
  summarise(n=n()) %>% 
  collect()

 hosp_codes %>% 
   arrange(-n) %>% 
write_csv("data/hosp_codes_lookup_raw.csv")
 
 hosp_codes_clean<- read_csv("data/hosp_codes_lookup_clean.csv") %>% 
   group_by(hospital_code, 
            campus_name,
            campus_code) %>% 
   filter(!is.na(campus_code)) %>%
   distinct(hospital_code, .keep_all = T) %>% 
   select(-hospital)

#patient transfers (start from jul 2018)
 transfer_times <- tbl(con,in_schema('vhirsvads_vw','ODS_PatTrans')) %>% 
   union_all(tbl(con,in_schema('vhirsvadshistory_vw','History_PatTrans'))) %>%
  filter(Trans_Flag == "Y",
         Case_Date_DT > as.Date("2015-01-01")) %>%
  select(at_scene_time = AtPat_DT,
         hosp_arr_time = AtDest_DT_PCR,
         hosp_triage_completed_time =Triage_Comp_DT,
         hosp_handover_completed_time = Handover_Comp_DT,
         ready_for_new_pat_time = Clear_DT_PCR,
         date = Case_Date_DT,
         hospital = DA_Details,
         hospital_code = DA_Loc_Cd) %>%
# filter(date == as.Date("2022-02-01")) %>%
  collect() %>% 
  mutate(
    hosp_handover_imputed = if_else(is.na(hosp_handover_completed_time),1,0),
    hosp_handover_completed_time = coalesce(hosp_handover_completed_time,ready_for_new_pat_time - (25*60)),
        recharge_time = pmax(0,ready_for_new_pat_time - hosp_handover_completed_time),
         transfer_time = pmax(0,hosp_handover_completed_time - hosp_triage_completed_time),
         triage_time = pmax(0,hosp_triage_completed_time - hosp_arr_time),
         transport_time = pmax(0,hosp_arr_time - at_scene_time),
         ramp_time_total = pmax(0,hosp_handover_completed_time - hosp_arr_time),
  ramp_40  = if_else(ramp_time_total<=(40*60),1,0)) %>%
  mutate(hour = hour(hosp_arr_time),
         hour_group = case_when(hour < 9 ~ 1,
                                hour < 17 ~ 2,
                                T ~ 3)) %>% 
   inner_join(hosp_codes_clean) %>% 
   filter(ramp_time_total < (12*60*60)) %>%
  group_by(campus_name,campus_code,date,hour_group,metro_regional) %>% 
  summarise(av_share_ramp_40 = mean(ramp_40),
            av_transfers = n(),
            av_ramp_time = mean(ramp_time_total),
            av_recharge_time = mean(recharge_time),
            transport_time = mean(transport_time)) 

##-- filter out covid periods 
clean_data<-  transfer_times %>% 
  left_join(callouts) %>% 
  mutate(day = weekdays(date, abbreviate = FALSE)) %>% 
  mutate(covid_periods = case_when(date < "2020-03-01" ~ "pre_Covid",
                                   date >= "2020-03-01" & date < "2022-02-15" ~ "Covid",
                                   date >= "2022-02-15" ~ "post_Covid")) %>%
  filter(covid_periods %in% c("pre_Covid", "post_Covid"),
         date>ymd("2018-06-30")) %>%
  group_by(date,day,hour_group, covid_periods) %>% 
  summarise(share_under_15 = weighted.mean(share_less_15_mins, av_requests, na.rm = TRUE),
            av_response_time = weighted.mean(av_response_time,av_requests, na.rm = TRUE),
            share_ramp_40 = weighted.mean(av_share_ramp_40, av_transfers, na.rm = TRUE),
            callouts = sum(av_requests, na.rm = TRUE),
            ramp_time_weight_mean = weighted.mean(av_ramp_time, av_transfers, na.rm = TRUE),
            recharge_time_mean = weighted.mean(av_recharge_time, av_transfers, na.rm = TRUE),
            transport_time = weighted.mean(transport_time,av_transfers,na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(av_response_time = as.integer(av_response_time )) %>%
  mutate(year_int = year(date) - 2017) %>%
  mutate(Mth = month(date))


library(statmod)
library(splines)


hist(clean_data)

##-- fit linear mod 
fit_lm <- lm(av_response_time^(1/3) ~ ramp_time_weight_mean + as.factor(covid_periods) + day + as.factor(hour_group)+callouts+transport_time, data = clean_data)

##-- mod outputs
aov_output <- car::Anova(fit_lm)
summary(fit_lm) # mod stats
plot(fit_lm) # residual plots

library(ggeffects)

##-- add model effect sizes 
callout_effects <- ggeffects::ggpredict(fit_lm, terms = c("callouts [100, 200, 300, 350, 353.5, 385, 400, 500]"))
ramping_effects <- ggeffects::ggpredict(fit_lm, terms = c("ramp_time_weight_mean [1400, 1600, 1800, 2000, 2020, 2200, 2400]"))
#transport_time_effects <- ggeffects::ggpredict(fit_lm, terms = c("transport_time [2000, 2020, 2200, 2400, 2600, 2800, 3000]"))



callouts_graph <- callouts %>% 
  mutate(day = weekdays(date, abbreviate = FALSE),
         year = year(date)) %>% 
  group_by(date,hour_group,day,year) %>% 
  summarise(av_requests = sum(av_requests)) %>% 
  filter(date>ymd("2018-06-30")) %>% 
  group_by(day,hour_group,year) %>% 
  mutate(diff = av_requests/mean(av_requests),
         demand = if_else(diff< 1.1 & diff > .9, "Within 20%","outside 20%")) %>% 
  mutate(type = "Ambulance callouts")
  
  callouts_graph %>% 
  group_by(demand) %>% 
  summarise(n = n()) %>% 
  mutate(x = n/sum(n))
  
  callouts_graph %>% 
  ggplot(aes(x = date, y = diff, colour = demand))+
  facet_grid(day~hour_group)+
  geom_point(stat = "identity")+
  labs(title = "AV requests for an ambualance are predictable, they are almost always within a 20% range for metro areas of Melbourne")

  
transfer_graph <- transfer_times %>% 
  mutate(day = weekdays(date, abbreviate = FALSE),
         year = year(date)) %>% 
  group_by(date,hour_group,day,year) %>% 
  summarise(av_ramp_time = mean(av_ramp_time, na.rm = TRUE),
            av_transfers = sum(av_transfers)) %>% 
  filter(date>ymd("2018-06-30")) %>% 
  group_by(day,hour_group,year) %>% 
  mutate(diff = av_ramp_time/mean(av_ramp_time),
         demand = if_else(diff< 1.1 & diff > .9, "Within 20%","outside 20%"))  %>% 
  mutate(type = "Ramping time")

transfer_number_graph <- transfer_times %>% 
  mutate(day = weekdays(date, abbreviate = FALSE),
         year = year(date)) %>% 
  group_by(date,hour_group,day,year) %>% 
  summarise( av_transfers = sum(av_transfers)) %>% 
  filter(date>ymd("2018-06-30")) %>% 
  group_by(day,hour_group,year) %>% 
  mutate(diff = av_transfers/mean(av_transfers),
         demand = if_else(diff< 1.1 & diff > .9, "Within 20%","outside 20%"))  %>% 
  mutate(type = "Transfers to major hospitals")


transfer_graph %>% 
  group_by(demand) %>% 
  summarise(n = n()) %>% 
  mutate(x = n/sum(n))

library(dhhstheme)
transfer_graph %>% 
  bind_rows(callouts_graph) %>% 
  ggplot(aes(x = date, y = diff, colour = fct_rev(demand)))+
  facet_grid(~type)+
  geom_point(stat = "identity")+
  labs(title = "AV requests for an ambualance are much more predictable than transfer of care time",
       y = element_blank(),
       colour = element_blank())+
  dhhstheme::theme_dhhs()+
  dhhs_colour_manual(n = 2)+
  dhhs_y_continuous(labels = )


clean_data %>% 
  group_by(hour_group) %>% 
  summarise(response_time = mean(av_response_time/60),
            callouts = mean(callouts))

library(absmapsdata)
library(sf)
sa1_mel <- sa12016 %>% filter(gcc_name_2016 == "Greater Melbourne") 

#How have callouts changed, controlling for population? Came to nothing because the pat trans numbers don't really match AV's website which sucks! 

abs_pop <- tbl(con,in_schema('abs','ESTIMATED_RESIDENT_POPULATION_SINGLE_YEAR_AGE_SEX_SA1')) %>% 
  select(-contains("Females"),-contains("Males"),-cdl_created_on,-cdl_partition_key,-cdl_partition_name,-contains("Total")) %>% 
  filter(`2016 Statistical Area Level 1` %in% local(sa1_mel$sa1_7dig_2016)) %>% 
  collect() %>% 
  pivot_longer(`Persons Aged 0`:`Persons Aged 85 or more`,
              names_to = "group", 
              values_to = "count") %>% 
  mutate(age = parse_number(group)) %>% 
  group_by(year=Year,age) %>% 
  summarise(count = sum(count)) 



callouts_by_age <- 
  tbl(con,in_schema('vhirsvads_vw','ODS_PatTrans')) %>% 
  union_all(tbl(con,in_schema('vhirsvadshistory_vw','History_PatTrans'))) %>% 
    select(Case_Date_DT,age = Age,Locality = Pat_Locality) %>% 
  collect() %>% 
  mutate(locality = tolower(Locality),
         month = floor_date(Case_Date_DT, unit = "months"),
         age = if_else(age>84,85,as.double(age))) %>% 
  inner_join(av_catchment) %>% 
  group_by(month,age) %>% 
  summarise(av_requests = n()) %>% 
  mutate(year = year(month))

callouts_by_age %>% 
  group_by(year) %>% 
  summarise(av_requests  =sum(av_requests  )) %>%  view()
  ggplot(aes(x = year, y = av_requests  ))+
  geom_line(stat = "identity")

callouts_by_age %>%
  mutate(year = if_else(year>2020,2020,year)) %>%
  left_join(abs_pop) %>% 
  mutate(request_rate = av_requests/count) %>% 
  filter(age>0) %>% 
  group_by(age) %>% 
  mutate(pop_av_across_dataset = mean(count)) %>% 
  group_by(month) %>% 
  summarise(requests = weighted.mean(request_rate,pop_av_across_dataset)) %>%
  ggplot(aes(x = month, 
             y = requests))+
  geom_line(stat = "identity")

#How has population changed? 
callouts_by_age %>%
  mutate(year = if_else(year>2020,2020,year)) %>%
  left_join(abs_pop) %>% 
  mutate(request_rate = av_requests/count) %>% 
  filter(age>0) %>% 
  group_by(age) %>% 
  mutate(request_rate_av = mean(request_rate)) %>% 
  group_by(year) %>% 
  summarise(population = weighted.mean(count,request_rate_av)) %>% 
  mutate(change = population /lag(population)) %>% view()


callouts_graph_v2 <- tbl(con,in_schema('vhirsvads_vw','ODS_Request')) %>% 
  union_all(tbl(con,in_schema('vhirsvadshistory_vw','History_Request'))) %>% 
  filter(Incident_Type == "EMG",
         Case_Date_DT > as.Date("2018-06-30"),
         !is.na(Der_Response_Time_Case),
         Priority_First %in% c("0","1"),
         Priority_Final %in% c("0","1"),
         #Team_TIG_inscope_flag == "Y"
  ) %>% 
  select(Case_Date_DT,
         Req_Locality,
         Disp_Final,
         Der_Response_Time_Case,
         Request_DT) %>% 
  collect() %>% 
  mutate(hour = hour(Request_DT),
         hour_group = case_when(hour < 9 ~ 1,
                                hour < 17 ~ 2,
                                T ~ 3),
         locality = tolower(Req_Locality)) %>% 
  left_join(av_catchment) %>% 
  mutate(metro_regional = if_else(is.na(hospital),"Regional","Metro"),
         month = floor_date(Case_Date_DT, unit = "months"),
         year = year(Case_Date_DT)) %>% 
 group_by(month, metro_regional, year) %>% 
  summarise(share_less_15_mins = mean(if_else(Der_Response_Time_Case<15*60,1,0)),
            callouts = n())

callouts_graph_v2%>% 
  ggplot(aes(x = month, y = share_less_15_mins, colour = metro_regional))+
  geom_line(stat = "identity")+
  theme_dhhs()+
  labs(y = "Share of cat 1 requests within 15 minutes",
       x = element_blank(),
       colour = "Region")+
  dhhs_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.5,1))+
  dhhs_colour_manual(n = 2)

ggsave("atlas/cat_1_within_15.png",dpi = 600, height = 4.5, width = 12.5)

callouts_graph_v2%>% 
  group_by(metro_regional) %>%
  mutate(change_2019 = callouts/mean(callouts[year == 2019]) ) %>% 
  ggplot(aes(x = month, y = callouts, colour = metro_regional))+
  geom_line(stat = "identity")+
  theme_dhhs()+
  labs(y = "AV requests",
       x = element_blank(),
       colour = "Region")+
  dhhs_y_continuous(labels = scales::number_format(big.mark = "," ),
                     limits = c(0,20000))+
  dhhs_colour_manual(n = 2)

ggsave("atlas/av_requests.png",dpi = 600, height = 4.5, width = 12.5)



#Ramping - change over time

transfer_times %>%
  filter(metro_regional == "Metro") %>%
  mutate(month = floor_date(date, unit = "months")) %>% 
  group_by(month,metro_regional) %>% 
  summarise(av_share_ramp_40  = weighted.mean(av_share_ramp_40 ,av_transfers)) %>% 
  ggplot(aes(x = month, y = av_share_ramp_40))+
  geom_line(stat = "identity")+
  theme_dhhs()+
  labs(y = "Share of ambulances ramping for less than 40 minutes",
       x = element_blank())+
  dhhs_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.5,.9))

ggsave("atlas/av_ramping.png",dpi = 600, height = 6.89, width = 15.7)

transfer_by_month_pre_post <- transfer_times %>%
  group_by(date,campus_name) %>% 
  summarise(av_share_ramp_40  = weighted.mean(av_share_ramp_40 ,av_transfers),
            av_transfers = sum(av_transfers)) %>% 
  mutate(month = floor_date(date, unit = "months"),
         month_group = if_else(month < ymd("2020-04-01"),"Pre-pandemic","Post-pandemic")) %>% 
  filter(month %in% c(ymd("2022-02-01"),ymd("2019-02-01"))) %>% 
  group_by(month,month_group,campus_name) %>% 
  summarise(av_share_ramp_40  = weighted.mean(av_share_ramp_40 ,av_transfers),
            av_transfers = sum(av_transfers))


transfer_by_month_pre_post_long <- transfer_by_month_pre_post %>% 
  ungroup() %>% 
  select(-month) %>%
  pivot_wider(names_from = month_group,
              values_from = c(av_share_ramp_40,av_transfers)) %>% 
  filter(`av_transfers_Post-pandemic` >1600,
         `av_transfers_Pre-pandemic` >1600,
         !str_detect(campus_name,"Vincents")) 

transfer_by_month_pre_post_long%>% 
  ggplot()+
  geom_segment(aes(x = fct_reorder(campus_name, `av_share_ramp_40_Post-pandemic`),
                   xend = fct_reorder(campus_name, `av_share_ramp_40_Post-pandemic`),
                   y = `av_share_ramp_40_Pre-pandemic`, 
                   yend = `av_share_ramp_40_Post-pandemic` ),
               stat = "identity",
               size = 5,
               colour = c(dhhs_pink2)) +
  geom_point(data = transfer_by_month_pre_post %>% filter(campus_name %in% transfer_by_month_pre_post_long$campus_name),
             aes(x = campus_name, 
                 y = av_share_ramp_40,
                 colour = fct_rev(month_group)),
             size = 5)+
  coord_flip()+
  theme_dhhs()+
  dhhs_colour_manual(n = 2)+
  dhhs_y_continuous(labels = scales::percent_format(accuracy = 1),
                    limits = c(.4,.95))+
  labs(x = element_blank(),
       colour = "Performance",
       y = "Share of transfer times less than 40 minutes")

ggsave("atlas/share_ramping_change.png",dpi = 600)

transfer_times %>% 
  mutate(month = floor_date(date, unit = "months")) %>% 
  group_by(campus_name,month) %>% 
  summarise(av_transfers = sum(av_transfers)) %>% 
  ggplot(aes(x = month, y = av_transfers))+
  geom_line(stat = "identity") +
  facet_wrap(~campus_name)

transfer_times %>% 
  mutate(month = floor_date(date, unit = "months")) %>% 
  group_by(month) %>% 
  summarise(av_transfers = sum(av_transfers)) %>% 
  ggplot(aes(x = month, y = av_transfers))+
  geom_line(stat = "identity") 



#transfer clumping at hospitals

transfer_graph_2 <- transfer_times %>% 
  mutate(day = weekdays(date, abbreviate = FALSE),
         year = year(date)) %>% 
  filter(date>ymd("2018-06-30")) %>% 
  group_by(day,hour_group,year,campus_name) %>% 
  mutate(diff = av_transfers /mean(av_transfers ),
         demand = if_else(diff< 1.1 & diff > .9, "Within 20%","outside 20%"))  %>% 
  mutate(type = "Ambulances received by each hospital")

av_ramp <- transfer_graph_2 %>% 
  filter(year == 2022) %>% 
  group_by(campus_name,campus_code) %>% 
  summarise(av_ramping_time = weighted.mean(av_ramp_time , av_transfers) /60)

transfer_ramp_graph <- transfer_graph_2 %>% 
  filter(year == 2022) %>% 
  group_by(demand,campus_name,campus_code) %>% 
  summarise(n = n(),
            ) %>% 
  left_join(av_ramp) %>% 
  group_by(campus_name) %>% 
  mutate(share_out_of_20_pc = n/sum(n)) %>% 
  filter(demand == "outside 20%")

transfer_ramp_graph%>% 
  ggplot(aes(x = av_ramping_time, y = share_out_of_20_pc))+
  geom_point(stat = "identity")+
  theme_dhhs() +
  geom_text(data = transfer_ramp_graph, aes(label = transfer_ramp_graph$campus_name))



transfer_graph %>% 
  bind_rows(transfer_graph_2%>% filter(campus_name == "Monash Medical Centre")) %>% 
  bind_rows(callouts_graph) %>% 
  bind_rows(transfer_number_graph) %>%
  # ungroup() %>% distinct(type)
  mutate(type = case_when(type == "Ambulance callouts" ~ "Ambulance requests in\nMetro Melbourne",
                          type == "transfer demand" ~ "AV transfers to\nall metro hospitals",
                          type == "Ambulances received by each hospital" ~ "Transfers received\nby MMC",
                          type == "Ramping time" ~ "Transfer time\nin metro Melbourne",
                          T ~ as.character(type))) %>% 
  mutate(type = fct_relevel(type, c("Ambulance requests in\nMetro Melbourne",
                                    "AV transfers to\nall metro hospitals",
                                    "Transfers received\nby MMC",
                                    "Transfer time\nin metro Melbourne"))) %>% 
  ggplot(aes(x = date, y = diff, colour = fct_rev(demand)))+
  facet_grid(~type)+
  geom_point(stat = "identity")+
  labs(title = "AV requests for an ambualance are much more predictable than ramping",
       y = element_blank(),
       x = element_blank(),
       colour = element_blank())+
  dhhstheme::theme_dhhs()+
  dhhs_colour_manual(n = 2)+
  dhhs_y_continuous(labels = scales::percent_format(accuracy = 1))

ggsave("atlas/av_requests_changes.png", dpi = 600, height = 7.5, width = 14)

transfer_graph_2 %>% 
  ggplot(aes(x = date, y = diff, colour = demand))+
  geom_point()+
  facet_wrap(~campus_name)

transfer_graph_2 %>% 
  group_by(campus_name,
           demand) %>% 
  summarise(n=n()) %>% 
  mutate(ratio = n/sum(n)) %>% 
  filter(demand == "outside 20%")
