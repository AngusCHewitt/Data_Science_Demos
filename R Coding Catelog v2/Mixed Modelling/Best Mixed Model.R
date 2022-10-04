##-- testing sampling distrn ability to predict or represent out of sample results --##
library(tidyverse)
library(lme4)


setwd("/Users/angushewitt/Desktop/AFL Datasets/")

##-- model dataset
load("AFL_Tabs_Model_DS.Rdata")

##-- best fitted model tabel
load("Best_Fit_Tab.Rdata") 

##-- build model features  

##-- corece factor into chars
AFL_tabs_dt %>%
  mutate(Career_GDRole_Cat = as.factor(Career_GDRole_Cat)) %>%
  mutate(Career_Position = as.factor(Career_Position)) %>%
  mutate(GameDay_Role = as.factor(GameDay_Role)) %>%
  mutate(No_Tall_Key_Frwds = if_else(No_Tall_Key_Frwds > 2,2,No_Tall_Key_Frwds)) %>%
  mutate(No_Ruckman_Play_Ruck = if_else(No_Ruckman_Play_Ruck > 2,2,No_Ruckman_Play_Ruck)) %>%
  mutate(No_Tall_Key_Frwds = as.factor(No_Tall_Key_Frwds)) %>%
  mutate(No_Ruckman_Play_Ruck = as.factor(No_Ruckman_Play_Ruck)) %>%
  mutate(lagged_weighted_TOG = round(lagged_weighted_TOG / 100,2)) %>%
  mutate(TS_Running_Med_TOG =  round(TS_Running_Med_TOG/ 100, 2)) %>%
  mutate(TS_Running_Range_TOG =  round(TS_Running_Range_TOG/ 100, 2 )) -> AFL_tabs_dt


##-- filter out first gamers
AFL_tabs_dt %>%
  dplyr::filter(!(is.na(lagged_Goals))) -> AFL_tabs_dt


##-- convert eyasr to integers values 
AFL_tabs_dt %>%
  mutate(yr_int = Season %% 100) %>%
  mutate(yr_intervals = case_when(yr_int <= 7 ~ "2004-07",
                                  yr_int > 7 & yr_int <= 11 ~  "2008-11",
                                  yr_int > 11 & yr_int <= 15 ~ "2012-15",
                                  yr_int > 15 ~ "2016+" )) %>%   
  mutate(yr_intervals = as.factor(yr_intervals))  %>%
  mutate(yr_int = as.factor(yr_int)) -> AFL_tabs_dt

## nest dataset at the game level - ensuring all player within the same game are included in the same pop
AFL_tabs_dt %>%
  group_by(game_ID) %>%
  nest() -> nest_game_level

# 50/50 training  and test set distrn
sample_size <- round(nrow(nest_game_level) * .95)



# keep 44 player of each game in the cross validated samples
set.seed(18) # 2 itr
nest_game_level %>%
  ungroup() %>%  
  sample_n(size = sample_size, replace = FALSE) %>%
  unnest() %>% 
  dplyr::select(ID, First.name, Surname, game_ID, lagged_Goals:yr_intervals, No_Ruckman_Play_Ruck , No_Tall_Key_Frwds) -> Training_Data


# edit not in data object function
'%!in%' <- function(x,y)!('%in%'(x,y))

##-- test dataset
AFL_tabs_dt %>%
  ungroup() %>%
  dplyr::filter(game_ID %!in% Training_Data$game_ID) %>%
  dplyr::select(ID, First.name, Surname, game_ID, lagged_Goals:yr_intervals, No_Ruckman_Play_Ruck , No_Tall_Key_Frwds) -> Test_Dataset


##-- glmer - Gameday form Rand Effect Fixed Intercept Career/GD_Role ** Best models **

##-- base line model with TS med as fixed coeff, TS range goals and Standard Career/GD:Height Cat
mixed_effects_TS_Coef <- glmer(HGS_Binary ~  TS_Running_Med_Goals + TS_Running_Med_Marks.Inside.50 + 
                                 (TS_Running_Range_Goals + TS_Running_Range_Marks.Inside.50  | Career_GDRole_Cat:Height_Categories) ,
                               data = Training_Data, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)


##-- Add fixed coeffs to the baseline ~ lagged_weighted_goals + lagged_weighted_Marks.Inside.50 
mixed_effects_TS_Coef1 <- glmer(HGS_Binary ~  TS_Running_Med_Goals + TS_Running_Med_Marks.Inside.50 + lagged_weighted_goals  + lagged_weighted_Marks.Inside.50 +
                                  (TS_Running_Range_Goals + TS_Running_Range_Marks.Inside.50  | Career_GDRole_Cat:Height_Categories) ,
                                data = Training_Data, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)


##-- used yr intervals to random slope, only TS Med Goals at fixed coeff, stardard rand effects. 
mixed_effects_TS_Coef2 <- glmer(HGS_Binary ~  TS_Running_Med_Goals  +
                                  ( TS_Running_Range_Goals:yr_intervals | Career_GDRole_Cat:Height_Categories) ,
                                data = Training_Data, family = binomial(link = "logit"),control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)


##-- Add fixed effects to yr interval mode, lagged_weighted_TOG + lagged_weighted_goals
mixed_effects_TS_Coef3 <- glmer(HGS_Binary ~   TS_Running_Med_Goals  + lagged_weighted_goals + lagged_weighted_TOG + 
                                  ( TS_Running_Range_Goals:yr_intervals | Career_GDRole_Cat:Height_Categories) 
                                ,data = Training_Data, family = binomial(link = "logit"),control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)

##-- Number of frwd model
mixed_effects_TS_Coef4 <- glmer(HGS_Binary ~   TS_Running_Med_Goals  + lagged_weighted_goals  + lagged_weighted_Marks.Inside.50 +
                                  ( TS_Running_Range_Goals:No_Tall_Key_Frwds | Career_GDRole_Cat:Age_Categories ),
                                data = Training_Data, family = binomial(link = "logit"),control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)

##-- Number of ruckman
mixed_effects_TS_Coef5 <- glmer(HGS_Binary ~  TS_Running_Med_Goals  + lagged_weighted_goals  + lagged_weighted_Marks.Inside.50 +
                                  + ( TS_Running_Range_Goals:No_Ruckman_Play_Ruck | Career_GDRole_Cat:Height_Categories) ,
                                data = Training_Data, family = binomial(link = "logit"),control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)


# pred player HGS prob using test data
mixed_effects_mod_baseline <- predict(mixed_effects_TS_Coef, newdata = Test_Dataset, type = "response")
mixed_effects_mod_baseline_TS_Coef1 <- predict(mixed_effects_TS_Coef1, newdata = Test_Dataset, type = "response")
mixed_effects_mod_baseline_TS_Coef2 <- predict(mixed_effects_TS_Coef2, newdata = Test_Dataset, type = "response")
mixed_effects_mod_baseline_TS_Coef3 <- predict(mixed_effects_TS_Coef3, newdata = Test_Dataset, type = "response")
mixed_effects_mod_baseline_TS_Coef4 <- predict(mixed_effects_TS_Coef4, newdata = Test_Dataset, type = "response")
mixed_effects_mod_baseline_TS_Coef5 <- predict(mixed_effects_TS_Coef5, newdata = Test_Dataset, type = "response")

# store mod predictions with Test Dataset - ten iteration and to use log likelihood of an obs being 0 or 1 
# -log(1 - P) prob not being a success and -log(p) prob of success
model_tabs <- data.frame(Baseline_Mod = mixed_effects_mod_baseline,
                          Baseline_Mod_TS_Coef1 = mixed_effects_mod_baseline_TS_Coef1, 
                          Baseline_Mod_TS_Coef2 = mixed_effects_mod_baseline_TS_Coef2, 
                          Baseline_Mod_TS_Coef3 = mixed_effects_mod_baseline_TS_Coef3, 
                          Baseline_Mod_TS_Coef4 = mixed_effects_mod_baseline_TS_Coef4,
                          Baseline_Mod_TS_Coef5 = mixed_effects_mod_baseline_TS_Coef5, 
                          Test_Dataset)
model_tabs %>%
  mutate(Baseline_Model_performance =  if_else(HGS_Binary == 0,-log(1-Baseline_Mod),-log(Baseline_Mod))) %>%
  mutate(TS_Coef1_performance =  if_else(HGS_Binary == 0,-log(1-Baseline_Mod_TS_Coef1),-log(Baseline_Mod_TS_Coef1))) %>%
  mutate(TS_Coef2_performance =  if_else(HGS_Binary == 0,-log(1-Baseline_Mod_TS_Coef2),-log(Baseline_Mod_TS_Coef2))) %>%
  mutate(TS_Coef3_performance =  if_else(HGS_Binary == 0,-log(1-Baseline_Mod_TS_Coef3),-log(Baseline_Mod_TS_Coef3))) %>%
  mutate(TS_Coef4_performance =  if_else(HGS_Binary == 0,-log(1-Baseline_Mod_TS_Coef4),-log(Baseline_Mod_TS_Coef4))) %>%
  mutate(TS_Coef5_performance =  if_else(HGS_Binary == 0,-log(1-Baseline_Mod_TS_Coef5),-log(Baseline_Mod_TS_Coef5))) -> model_tabs


# gather all model performance stats
model_tabs %>%  
  gather("Model","Performance", Baseline_Model_performance:TS_Coef5_performance) %>%
  mutate(Career_GDRole_Cat = as.character(Career_GDRole_Cat)) %>%
  mutate(Height_Categories = as.character(Height_Categories)) %>%
  mutate(Highest_Goal_Scorer = as.character(Highest_Goal_Scorer)) -> sum_tabs

str(Best_Fit_Tab)

Best_Fit_Tab %>%
  select(Career_GDRole_Cat, Height_Categories, Highest_Goal_Scorer, Model, flag_best_fit) -> Sub_Best_Fit

## add best model flag to summary tabs
  sum_tabs %>%
  left_join(Sub_Best_Fit, by = c("Career_GDRole_Cat" = "Career_GDRole_Cat",
                              "Height_Categories" = "Height_Categories",
                              "Highest_Goal_Scorer"  = "Highest_Goal_Scorer" ,
                               "Model" = "Model")) -> sum_tabs

  
##-- examine model performance using log likehood methology   
  sum_tabs %>%
    dplyr::filter(Highest_Goal_Scorer == "HGS") %>%
    dplyr::filter(Career_GDRole_Cat == "Key Forward_Key Forward") %>%
    ggplot(aes(x = Model, y = Performance, fill = flag_best_fit)) + geom_boxplot() + facet_grid(~ Height_Categories)
  






rm(list = ls())  # caution: delete all objects in .GlobalEnv
gc()  # free system memory




