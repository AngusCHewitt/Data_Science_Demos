# Renal Dialysis Mod

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
library("qcc")
library("GGally")
library(forecast)
library(seasonal)

sur_dat <- read_csv("Combine Renal Register Data.csv")


Dataset <- readXL("//internal.vic.gov.au/DHHS/HomeDirs6/ahew2812/Documents/VIC POP.xlsx", rownames=FALSE, header=TRUE, 
                  na="", sheet="Sheet1", stringsAsFactors=TRUE)

str(Dataset)

# tidy data: gather all the year into 1 col
Dataset %>%
  gather("Year","Pop", -Month) %>%
  mutate(obs = 1:240) -> VIC_Pop



# Select only var. needed for sur analysis
sur_dat %>%
  mutate(date = dmy(EOM)) %>%
  mutate(month_year = as.yearmon(date)) %>%
  mutate(ReasonRemoved = as.factor(ReasonRemoved)) %>%
  mutate(obs = 1) %>%
  filter(is.na(ReasonRemoved)) %>%
  mutate( DxMode = as.factor(DxMode)) %>%
  select(UrNumber, DxMode, date, month_year, obs ) -> perf_dat


# label modlity modes into HD or Home 
perf_dat %>%
  mutate (Modality = case_when(DxMode == 'APD'  ~ "Home",
                               DxMode == 'CAPD'  ~ "Home",
                               DxMode == 'HHD'  ~ "Home",
                               DxMode == 'HNHD'  ~ "Home",
                               
                               DxMode == 'IHD'  ~ "HD",
                               DxMode == 'NHD'  ~ "HD",
                               DxMode == 'PD'  ~ "HD",
                               DxMode == 'SHD'  ~ "HD",
                               DxMode == 'SNHD'  ~ "Home",
                               
                               is.na(DxMode)  ~ "Unknown")) %>%
  mutate(Modality = as.factor(Modality)) -> perf_dat_Mods


# H.D. renal patients 
perf_dat_Mods %>%
  group_by(month_year,Modality) %>%
  summarise(pat_cnt = sum(obs)) %>%
  filter(year(month_year) > 2010 ) %>% 
  filter(Modality != "Unknown") %>%
  spread(key = Modality, value = pat_cnt) -> HD_pat_cnts

# VIC POP Jan 2011 to Mar 2019

VIC_Pop %>% filter(obs > 96 & obs < 196 ) -> Sample_pop
  

# fit a linear mod
mod_df <- data.frame(obs = 1:99, HD_pat_cnts,pop = Sample_pop$Pop)

fit <- lm(HD ~ obs + Home + pop, data = mod_df )

summary(fit)

# seasonal pattern exhibited in to residuals
plot(residuals(fit))


# coerce into t.s.
renal_ts <- ts(mod_df, start = c(2011,1),frequency = 12)

exp_vars <- data.frame(obs = mod_df$obs, HD =  mod_df$Home, Pop = mod_df$pop)

mat_exp_vars <- as.matrix(exp_vars)

# fit linear mod with ARIMA Errors
ts_mod <- tslm(HD ~ Home + trend + season + pop , data = renal_ts)

summary(ts_mod)

checkresiduals(ts_mod)


acf(diff(mod_df$HD))


(fit <- auto.arima(mod_df$HD,
                   xreg=mat_exp_vars))


checkresiduals(fit)
