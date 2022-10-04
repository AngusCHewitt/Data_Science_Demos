#-- Survival analysis of Renal Reg dataset --#

#-- use the mod to make inference on the Private Renal Patients 

library("survival")
library("zoo")
library("modelr")
library("tidyverse")
library("gapminder")
library("forecast")
library("tidyquant")
library("timetk")
library("sweep")
library("lubridate")

sur_dat <- read_csv("Survival Analysis Renal Registry.csv")

dat <- read.csv("Survival Analysis Renal Registry.csv")


# Select only var. needed for sur analysis
sur_dat %>%
  mutate(date = dmy(date)) %>%
  mutate(month_year = as.yearmon(date)) %>%
  mutate(DOB = mdy(DOB)) %>%
  mutate(Modality = as.factor(Modality)) %>%
  mutate(ReasonRemoved = as.factor(ReasonRemoved)) %>%
  mutate( Age = year(date) - year(DOB)) %>%
  
  select(Uni_ID,check_flag_first_dialysis_month, check_flag_last_dialysis_month,
             Modality, ReasonRemoved, Age,  DOB, Sex, date, month_year ) -> contcat_dat

# find the duration of treatment and create censored field - need to adjust, for censored obs (i.e. not last month)
contcat_dat %>%
  mutate(Pathway = case_when(check_flag_first_dialysis_month == 1 ~ "min",
                   check_flag_last_dialysis_month == 1 ~ "max",
                   TRUE                      ~  "other")) %>%
  mutate(Pathway = as.factor(Pathway)) -> range_dat

# calc the difference between the first month and last month of treatment
# create a flat file for each unique ID

# flagging patient is deceased or pallative care (1) or censored (0)
range_dat %>%
 mutate (Stage = case_when(check_flag_last_dialysis_month == 1 &  ReasonRemoved ==  "DEC" ~ 1,
                      check_flag_last_dialysis_month == 1 &  ReasonRemoved ==  "ded" ~ 1,
                      check_flag_last_dialysis_month == 1 &  ReasonRemoved ==  "DED" ~ 1,
                      check_flag_last_dialysis_month == 1 &  ReasonRemoved ==  "PAL" ~ 1,
                      TRUE                      ~  0))  -> Add_stages


# Add var. to capture if patients had a transplant
Add_stages %>%
  mutate (transplant = case_when(ReasonRemoved ==  "TXD" ~ 1,
                            ReasonRemoved ==  "TXL" ~ 1,
                            TRUE                      ~  0))  -> Add_txt


  
# create a nested dataset to combine surv info.

# nest data of each year to create a flat file (by SA4, Diabetes)
Nest_dataset <- Add_txt %>% 
  group_by(Uni_ID) %>% 
  nest()
Nest_dataset$data[[4]]



# fun. calc treatment duration
Mod <- function(df) {

  df$Modality[1]

}

# fun. calc min. Age
Age <- function(df) {
  
  min(df$Age)
  
}


# reason for removal
levels(range_dat$ReasonRemoved)


# fun. censored var. (1 = death, 0 = censored)
Stages <- function(df) {
  

dat = ifelse(df$check_flag_last_dialysis_month == 1 & df$Stage == 1,1,0)
 
as.tibble(dat)
  
}



# Nested survival dataset 
Nest_dataset1 <- Nest_dataset %>% 
  mutate(Mod = map(data, Mod))%>%
  mutate(Age = map(data, Age)) %>%
  mutate(Stages = map(data, Stages))  



# fun. transplants
transplant_fun <- function(df) {
  
  max(df$transplant)
  
}

# fun. censored var. (1 = death, 0 = censored)
# add censored var. and transplants 
# Tranplants (1 = Had Transplant, 0 = No recorded Transplant)

Nest_dataset2 <- Nest_dataset1 %>% 
  mutate(Censored = map(Stages, max)) %>%  
  mutate(Trans = map(data, transplant_fun))  

# check whether a patient had a transplant
Nest_dataset2$Trans[3]


UnNest_dataset3 <- Nest_dataset2 %>% 
  mutate(Censored = map(Stages, max)) %>%  
  mutate(Trans = map(data, transplant_fun)) %>%  
  unnest(Mod) %>%
  unnest(Trans) %>%
  unnest(Censored) %>%
  unnest(Age) 

# Add treatment duration using min and max date

UnNest_dataset3$data[1]

# fun. transplants
Treatment_Mths <- function(df) {
  
  range = max(df$date) - min(df$date)
  
  range_num <- as.numeric(range)
  
  (range_num / 365) * 12
  
}

# add duration to nested d.s.
UnNest_dataset4 <- UnNest_dataset3 %>% 
  mutate(Duration_Mths = map(data, Treatment_Mths)) %>%
  unnest(Duration_Mths)


# explore patients duration 

# select only unnested vars
UnNest_dataset4 %>%
  select( Mod:Duration_Mths) %>%
  mutate ( Transplants = as.factor(Trans)) %>%
  mutate (Cen = as.factor(Censored)) -> tidy_renal_ds

# dataset must have left had side censoring - not all patients are listed as dying


#-- explore patient pathways - start and end point --#

# First and last patient stage 


# fun. frst and last steps
last_step <- function(df) {
  
  len <- length(df$ReasonRemoved)
  df$ReasonRemoved[len]
}


#length(as.data.frame(UnNest_dataset5$data))

# add duration to nested d.s.
UnNest_dataset5 <- UnNest_dataset4 %>% 
  mutate(Last_stage = map(data, last_step)) %>%
  unnest(Last_stage) %>%
  select( Mod:Last_stage) %>%
  mutate ( Transplants = as.factor(Trans)) %>%
  mutate (Cen = as.factor(Censored)) -> tidy_renal_ds

#
UnNest_dataset4

