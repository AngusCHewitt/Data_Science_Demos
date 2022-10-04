library(magrittr)
library(tidyverse)
library(lubridate)
library(vcd)
library(zoo)


# --- Analysis of the effect of Supervised gym on hand-on treatmenyts ---- #

#read in dataset as a tibble 
 Remedial <- read_csv("REMEDIAL_SURG_LST_ESP.csv") # coerce dataframe into tibble
  
# coerce - date fields, year as factor
## create category for date service quaryter & year 
  
  Remedial  %>% 
   mutate(dateService = dmy(dateService))%>%
   mutate(first_s_gym_dt=  dmy(first_s_gym_dt))%>%
   mutate(first_esp_year= year(first_s_gym_dt))%>%
   mutate(year_service = year(dateService))%>%
   mutate(Service_y_qtr = as.yearqtr(dateService))%>%
   mutate (time_intervals = ifelse(Service_y_qtr < 2015.1,"Pre_2015",Service_y_qtr))%>%
   mutate(FrstEsp_y_qtr = as.yearqtr(first_s_gym_dt))%>%
   mutate (time_intervals = as.factor(time_intervals)) %>%
   mutate (Service_cate = as.factor(Service_cate)) %>%
   mutate (Service_Cate_Gym = as.factor(Service_Cate_Gym)) %>%
   mutate (Service_type  = as.factor(Service_type)) %>%
  mutate (Streamline_Pop = as.factor(Streamline_Pop))-> subset_Remedial

# create random sample of 10 claims
  subset_Remedial %>%
  select(CID) %>%
  filter(! duplicated(CID))%>%
  sample_n(10)  -> unique_list

# join sample with original d.s
  subset_Remedial %>% 
  inner_join(unique_list,by="CID") -> join_sample

# reorder time_intervals factor levels 
  join_sample$time_intervals <- with( join_sample, factor(time_intervals, 
  levels=c('Pre_2015','2015.1','2015.2','2015.3','2015.4','2016.1','2016.2',
   '2016.3','2016.4','2017.1','2017.2','2017.3','2017.4','2018.1','2018.2')))

# summarise by CID, time interval and service_type
join_sample %>%  
group_by(CID, Service_y_qtr,Service_type) %>%
summarise(claims = sum(length(invoices), na.rm = TRUE)) -> summarise_remedial


# tranpose dataset and move time_intervals levels from rows to columns
   summarise_remedial %>%
    spread(Service_type,claims) -> spread_remedial

# add a col with row numbers 
   spread_remedial$rownumber = 1:nrow(spread_remedial)

# find last service quarter for each claimant
   spread_remedial %>%
     group_by(CID)%>%
      summarise(last_date = max(rownumber)) -> last_row

# create subset with just claim id and first esp qtr and no duplicate rows 

subset_Remedial %>%
 select(CID,FrstEsp_y_qtr)%>%
  filter(! duplicated(CID)) -> subset_first_qtr


# join last row dataset with spread remedial ds., 
## first esp qtr, last esp qtr & create first qtr categories
 

spread_remedial %>%
  inner_join(last_row,by="CID") %>%
  mutate(last_date = as.integer(last_date)) %>%
  mutate(last_qtr  = ifelse(last_date == rownumber,"Last_qtr","Prior"))  %>%
  inner_join(subset_first_qtr,by="CID") %>%
  mutate(First_esp_cate = case_when(
  Service_y_qtr < FrstEsp_y_qtr ~"Pre_First_Qtr",
  Service_y_qtr == FrstEsp_y_qtr ~ "First_Qtr",
  Service_y_qtr > FrstEsp_y_qtr ~ "Post_First_Qtr")) %>%
  mutate(rowcount = n()) -> add_lstrow

# calculate the difference between 2 qtr dates
add_lstrow %>%
mutate(date = ((FrstEsp_y_qtr) - (Service_y_qtr))*4) -> test

View(test)

# need to adjust last surg date so only pick up last 2 qtrs prior
# allow for 6 months experience prior to first supervised gym service
 test %>%
   filter(date <= 4) -> Subset_Exp


# weeks
difftime(strptime("26.03.2014", format = "%d.%m.%Y"),
strptime("14.01.2013", format = "%d.%m.%Y"),units="weeks")
Time difference of 62.28571 weeks

# months
(as.yearmon(strptime("26.03.2014", format = "%d.%m.%Y"))-
as.yearmon(strptime("14.01.2013", format = "%d.%m.%Y")))*12
[1] 14

# quarters
(as.yearqtr(strptime("26.03.2014", format = "%d.%m.%Y"))-
as.yearqtr(strptime("14.01.2013", format = "%d.%m.%Y")))*4
[1] 4

# years
year(strptime("26.03.2014", format = "%d.%m.%Y"))-
year(strptime("14.01.2013", format = "%d.%m.%Y"))
[1] 1




