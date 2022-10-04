# connect to renal registry SQL database

library(sqldf)
library(RODBC)
library(tidyverse)
library(lubridate)
library(zoo)
library(DataCombine)
library(broom)
library(forecast)
library(RcmdrMisc)


## -- Renal Registry SQL Dataset --##

# connet to sql server - "time series"
con <- odbcConnect("TimeSeries")

# close connection
# odbcClose(con)

# connection Sql renal registry d.b.
renal_ds <- sqlQuery(con,"SELECT * FROM [RENAL_DIALYSIS_RPT].[fact].[REGISTRY_DATA]")


# location names
map_tb <-  sqlQuery(con,"SELECT * FROM [RENAL_DIALYSIS_RPT].[dim].[LGA_LOOKUP]")

join_maps <- renal_ds %>% left_join(map_tb, by =  "LGA") 


str(join_maps)

# add yeah month
join_maps %>%
  mutate(EOM_yr_mth = as.yearmon(as.Date(EOM))) %>%
  mutate(obs = 1) %>%
  filter(is.na(ReasonRemoved)) %>%
  group_by(`RDV Partnership`,LGA ,EOM_yr_mth) %>%
  summarise(no.patients = sum(obs)) -> EOM_add_ds

# visualise RDV partner ships
#ggplot(data = EOM_add_ds, aes(x = no.patients)) + geom_density() + facet_wrap(~`RDV Partnership` )


# spread month to at 0's instead of NA's
EOM_add_ds %>%
  spread(EOM_yr_mth,no.patients)  -> spread_mths


# nest d.s.
spread_mths %>%
  group_by(`RDV Partnership`, LGA) %>%
  nest() -> nest_EOM_ds

# No. of row function - pass through pmaps
na_remove <- function (df) { 
  ts = ifelse(is.na(df),0,df)}


# replace na's with zero values
nest_EOM_ds %>%
  mutate(data_ts = purrr::map(data, na_remove)) %>%
  unnest(data_ts) %>%
  unnest(data_ts) %>%
  group_by(`RDV Partnership`,LGA) %>%
  nest() -> LGA_renal_Patient


# 16 levels of RDV partnership
LGA_renal_Patient$data[16]
# gather t.s. to get dates - 176 months
# add date objects
# %>% 


# add dates to d.s.


# function to create d.fs
fun_df <- function(df) {
  data = data.frame(df) }

# add d.f.
LGA_renal_Patient %>%
  mutate(data_dfs = purrr::map(data,fun_df)) -> add_df_data

# 2056 + (2074 - 2056) / 2


# add month Apr 2006 to each LGA 
Lin_interpolation <- function (df) { 
  April_2006 = df$data_ts[20] + (df$data_ts[20] - df$data_ts[18]) / 2
  data <- InsertRow(data = df, NewRow = April_2006, RowNum = 19)
}

# Add data for apr 2006 for each LGA
add_df_data %>%
  mutate(data_ts = purrr::map(data_dfs,Lin_interpolation)) -> add_april_data

# tidy t.s. remove from jul 2005

# function to create d.fs
tidy_ts <- function(df) {
  
  data = data.frame(patient_counts = df$data_ts[10:177]) 
  obs = (1:168)
  data.frame(obs,data)}


# Add subset from Jul 2006 and date variable
add_april_data %>%
  mutate(data_tidy_ts = purrr::map(data_ts,tidy_ts))%>%
  unnest(data_tidy_ts, .drop = TRUE) %>%
  filter(`RDV Partnership` !='INTERSTATE/OTHER') -> tidy_renal_ts


## -- DEWLP Population dataset --##


# read in excel file
Dataset <- readXL("//internal.vic.gov.au/DHHS/HomeDirs6/ahew2812/Documents/DELWP pop data.xlsx",
                  rownames=FALSE, header=TRUE, na="", sheet="Sheet1", stringsAsFactors=TRUE)


head(Dataset)

Dataset %>%
  gather("Month","LGA_Pop",January:December) %>%
  mutate(Month = as.factor(Month)) %>%
  mutate(Year = as.factor(Year)) -> tidy_dates


# re-order months
tidy_dates$Month <- with(tidy_dates, factor(Month, levels=c('January',
                                                            'February','March','April','May','June','July','August','September',
                                                            'October','November','December')))

# sort dataset by LGA, year, month
tidy_dates %>%
  arrange(Local.Government.Area, Year, Month) %>%
  mutate(obs = rep(1:312, 79)) -> sort_ds



# nest sorted data to reduce montly pop
sort_ds %>%
  group_by(LGA.code) %>% nest() -> nest_sort_ds

#data.frame(nest_sort_ds$data[1])


# fun. grab same dates as renal register
filter_dates <- function(df){
  
  filter(.data = df, obs >= 7 & obs < 175)}

# create nested subset
nest_sort_ds %>%
  mutate(subset_ts = map(data, filter_dates)) -> nest_subset

# unnest subsets
nest_subset %>%
  unnest(subset_ts, .drop = TRUE) %>%
  mutate(obs = rep(1:168,79)) -> unnest_subset


# join datasets pop and renal counts, left join to remove LGA from population
tidy_renal_ts %>% left_join(unnest_subset, by = c( "LGA" =  "LGA.code",
                                                   "obs" = "obs")) -> join_pop

# nest dataset by LGA
join_pop %>% 
  group_by(`RDV Partnership`,Year,Month) %>%
  summarise(RDV_Pop = sum(LGA_Pop),RDV_patient_counts = sum(patient_counts)) %>% 
  group_by(`RDV Partnership`) %>%
  nest() -> nest_combine_ds

# rescale pop and renal dialysis 
rescale <- function(df) {
  scale_data  = data.frame(scale(df[3:4]))}

nest_combine_ds %>%
  mutate(data_scale = map(data,rescale)) -> nest_combine_ds


unnest_ds <- nest_combine_ds %>% unnest(data_scale)

ggplot(data = unnest_ds, aes(x = RDV_Pop, y = RDV_patient_counts) ) + geom_point() + facet_wrap(~`RDV Partnership`)

saveRDS(nest_combine_ds,"nest_combine_rdv_ds_.rds")

# create perm rdata fcast dataset
nest_combine_ds <- readRDS("//internal.vic.gov.au/DHHS/HomeDirs6/ahew2812/Desktop/R Code/nest_combine_ds.rds")


