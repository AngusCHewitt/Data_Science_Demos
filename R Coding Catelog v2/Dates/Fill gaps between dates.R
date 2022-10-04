##-- fill gaps between dates using the last rec obs for each missing var.

library(tidyverse)
library(tsibble)
library(haven)
library(stringr)

##-- read in SAS daatset
chris3 <- read_sas("~/OneDrive - Department of Health and Human Services. Victoria/Azure automation project/chris3.sas7bdat")
chris4 <- read_sas("~/OneDrive - Department of Health and Human Services. Victoria/Azure automation project/chris4.sas7bdat")
chris_final <- read_sas("~/OneDrive - Department of Health and Human Services. Victoria/Azure automation project/chris_final.sas7bdat")


glimpse(chris3)

##-- arrange by grouping and dates to ensure correct seq order of fields
##-- use row ID to filter out duplicate values by the key and index ids
chris3 %>%
  arrange(Campus_Code, CHRIS_DASHBOARD_CampusName, Field_Name, Effective_From) %>%
  mutate(row_no = 1:nrow(chris3)) %>%
  duplicates(key = c(Campus_Code, CHRIS_DASHBOARD_CampusName, Field_Name), index = Effective_From) -> dup_values

# edit not in data object function
'%!in%' <- function(x,y)!('%in%'(x,y))

##-- arrange by grouping and dates to ensure correct seq order of fields, rowid will be used to id last rec obs
##-- filter out dup values using value not in vector fun
chris3 %>%
  arrange(Campus_Code, CHRIS_DASHBOARD_CampusName, Field_Name, Effective_From) %>%
  mutate(row_no = 1:nrow(chris3)) %>%
  filter(row_no %!in% dup_values$row_no) %>%
  mutate(date = as.Date(Effective_From)) %>%
  group_by(Campus_Code, CHRIS_DASHBOARD_CampusName, Field_Name, date) %>%
  nest() %>%
  mutate(max_row_id = map_dbl(data, ~max(.$row_no))) %>%
  unnest() %>%
  ungroup() %>%
  filter(row_no == max_row_id) %>%
  select(-Effective_To, -Effective_From, -row_no, -max_row_id) -> chris_no_dups

##-- convert data obj into tsibble and group by campus, field name with index = effective_from
##-- dims = 181330 * 5
 chris_no_dups %>%
  tsibble(key = c(Campus_Code, CHRIS_DASHBOARD_CampusName, Field_Name), index = date) %>% 
  group_by_key() %>%
  fill_gaps() %>%
  tidyr::fill(Field_Value, .direction = "down") %>%
  ungroup() %>%
  as.tibble() -> chris_no_dups_fill_gaps
 
 table(chris_no_dups_fill_gaps$Field_Value)

 
 
 ##-- label field names and spread to a wide dataset
 ##-- amend PEE Field values
 chris_no_dups_fill_gaps %>%
    mutate(new_Field_names = case_when(Field_Name == "ICU patients" ~ "ICU_patients",
                                  str_starts(Field_Name,"HDU") ~ "HDU_patients",
                                  Field_Name == "Invasive ventilation" ~ "IV_patients",
                                  Field_Name == "Non-invasive ventilation" ~ "NIV_patients",
                                  Field_Name == "Confirmed COVID ‘+’ cases in your ICU/HDU(s)" ~ "COVID_patients",
                                  Field_Name == "Confirmed COVID ‘+’ cases in your ICU requiring Invasive Ventilation" ~ "COVID_IV_patients",
                                  Field_Name == "Renal replacement therapy" ~ "CRRT_patients",
                                  Field_Name == "ECMO" ~ "ECMO_patients",
                                  str_starts(Field_Name,"Patients awaiting admission to HDU") ~ "Awaiting_HDU",
                                  str_starts(Field_Name,"Patients awaiting admission to ICU") ~ "Awaiting_ICU",
                                  Field_Name == "Physical ICU capable bed spaces" ~ "Physical_ICU_spaces",
                                  str_starts(Field_Name,"Presently") ~ "Available_ICU_beds",
                                  Field_Name == "Spare dialysis/filters" ~ "Spare_dialysis",
                                  Field_Name == "Spare ventilators" ~ "Spare_ventilators",
                                  str_starts(Field_Name,"Critical care medical") ~ "Staff_absent_COVID",
                                  str_starts(Field_Name,"ICU PPE stock") ~ "ICU_PPE",
                                  TRUE ~ Field_Name)) %>%
   mutate(Field_Value = case_when(str_starts(Field_Value, "Less than 3") ~ "Under 3 days",
                        str_starts(Field_Value, "Run out in 4") ~ "Under 48 hours",
          TRUE ~ Field_Value)) -> chris_no_dups_fill_gaps
 
 
 # return first row of every nest value
 fist_obs_fun <- function(df) {
   df[1,]}
 
 
 ##-- remove dup 40 duplicate obs 
 chris_no_dups_fill_gaps %>%
   select(-Field_Name) %>%
   group_by(date, Campus_Code, CHRIS_DASHBOARD_CampusName, new_Field_names) %>%
   nest() %>%
   mutate(remove_dups = map(data, ~fist_obs_fun(.))) %>%
   select(-data) %>%
   unnest() %>%
   ungroup() -> chris_no_dups_fill_gaps_first_value
 
 
 ##-- spread dataset from long to wide
 chris_no_dups_fill_gaps_first_value %>%
   spread(new_Field_names, Field_Value) %>%
   select(-`Confirmed COVID ‘+’ cases admitted to your hospital`) %>%
   mutate(today = if_else(date == lubridate::today(), "Yes","No")) -> chris_final_test
 
 
 ##-- write out final chris dt
 write.csv(chris_final_test, "chris_final.csv")


##------------------ Sense Checks feilds with gaps -------------##
 
 
 ##-- notes 
 ## how is the field "Confirmed COVID ‘+’ cases admitted to your hospital" being accounted for
 ## Which PEE stock values should be selected if there are multiple values on the same day (may need to keep time var)
 ## Need to create a nest loop to sum HDU and ICU values as two fields can be selected on the day (char values)
 ## Potential for dup values in this new field names: Staff_absent_COVID HDU_patients Awaiting_HDU HDU_patients ICU_PPE Available_ICU_beds
 # table(chris_no_dups_fill_gaps$Field_Name, chris_no_dups_fill_gaps$new_Field_names)
 # missing campname where does that var come in 
 # also dont have a flag for today 
 
 
 
 ##-- rename fill gaps 
 chris_no_dups_fill_gaps %>%
   rename("Field_Value_no_Gaps" = Field_Value) -> chris_no_dups_fill_gaps
 
 ##-- left join fill gaps to dataset with gaps for testing comparison
 chris_no_dups_fill_gaps %>%
   left_join(chris_no_dups, by = c("Campus_Code" = "Campus_Code",
                                   "CHRIS_DASHBOARD_CampusName" = "CHRIS_DASHBOARD_CampusName",
                                   "Field_Name" = "Field_Name",
                                   "date" = "date")) -> chris_no_dups_fill_gaps_testing
 
 ##-- dt with all the missing field values
 chris_no_dups_fill_gaps_testing %>%
   filter(is.na(Field_Value)) -> missing_field_values
 
 ##-- grap sample of 10 values for comparison
 testing_Sample <-  sample_n(missing_field_values, 10)
 
 
 testing_Sample %>%  
   mutate(new_Field_names = case_when(Field_Name == "ICU patients" ~ "ICU_patients",
                                      str_starts(Field_Name,"HDU") ~ "HDU_patients",
                                      Field_Name == "Invasive ventilation" ~ "IV_patients",
                                      Field_Name == "Non-invasive ventilation" ~ "NIV_patients",
                                      Field_Name == "Confirmed COVID ‘+’ cases in your ICU/HDU(s)" ~ "COVID_patients",
                                      Field_Name == "Confirmed COVID ‘+’ cases in your ICU requiring Invasive Ventilation" ~ "COVID_IV_patients",
                                      Field_Name == "Renal replacement therapy" ~ "CRRT_patients",
                                      Field_Name == "ECMO" ~ "ECMO_patients",
                                      str_starts(Field_Name,"Patients awaiting admission to HDU") ~ "Awaiting_HDU",
                                      str_starts(Field_Name,"Patients awaiting admission to ICU") ~ "Awaiting_ICU",
                                      Field_Name == "Physical ICU capable bed spaces" ~ "Physical_ICU_spaces",
                                      str_starts(Field_Name,"Presently") ~ "Available_ICU_beds",
                                      Field_Name == "Spare dialysis/filters" ~ "Spare_dialysis",
                                      Field_Name == "Spare ventilators" ~ "Spare_ventilators",
                                      str_starts(Field_Name,"Critical care medical") ~ "Staff_absent_COVID",
                                      str_starts(Field_Name,"ICU PPE stock") ~ "ICU_PPE",
                                      TRUE ~ Field_Name)) %>%
   mutate(Field_Value = case_when(str_starts(Field_Value, "Less than 3") ~ "Under 3 days",
                                  str_starts(Field_Value, "Run out in 4") ~ "Under 48 hours",
                                  TRUE ~ Field_Value)) -> testing_Sample
 
 
 
 
 
# chris_final_test$COVID_patients
 
 chris_no_dups_fill_gaps %>%
   filter(CHRIS_DASHBOARD_CampusName == "Royal Childrens" & date == "2020-04-09") %>% 
   View()
 

 ##-- dupicate value HDU and ICU
 chris_no_dups_fill_gaps %>%
   filter(CHRIS_DASHBOARD_CampusName == "Austin" & date == "2020-04-15") %>% 
   select(-Field_Name) %>%
   group_by(Campus_Code, CHRIS_DASHBOARD_CampusName, date, new_Field_names) %>%
   nest() 
 
 
  chris_no_dups %>%
  mutate(test = 1) %>%
  right_join(chris_no_dups_fill_gaps) %>%
  filter(is.na(test)) %>%
  tail()

##-- spot check known missing values  
  chris4 %>%
    arrange(Campus_Code, CHRIS_DASHBOARD_CampusName, Field_Name, date) %>%
    filter(Campus_Code == "8550" & Field_Name == "Spare ventilators") %>%
    View()
  
##-- view data 
  glimpse(chris3)
  
  attach(chris3)
  length(unique(c(Campus_Code, CHRIS_DASHBOARD_CampusName, Field_Name, Effective_From)))
  # 47 unique campuses
  
  sum(is.null(chris3$Campus_Code))  
  
  ##-- R's step 4 still has duplicate values
  chris4 %>%
  duplicates(key = c(Campus_Code, CHRIS_DASHBOARD_CampusName, Field_Name), index = date)


