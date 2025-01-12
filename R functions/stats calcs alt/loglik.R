##-- Find best fitting mixture model for each gameday role category --##

library(tidyverse) # tidy data
library(lme4) #mixed mods
library(modelr) # many mods
library(broom.mixed) # tidy model vars
library(effects) # visualise models effects
library(statmod)# mod stats 
library(ggeffects) # plot mixd effects
library(splines) # natrtual spline NL relationship
library(sjPlot) #mixed effects plots
library(rattle) # cate binning 
#  library(mgcv) #  gam mods
#  library(DHARMa) # mixed model residual test and visuals 
library(ordinal) # cum link mixed modds 

load("data/curated_dts_Refreshed/step_5_away_Home_SP_bullpen_Mod_vars.rds") # SP abd bullpen var 
load("data/curated_dts_Refreshed/step_2_home_Away_Batting_Rolling_mod_Vars.rds") # bat var

target_Var = "7.5" 


## Fun to calc loglik for above target var
data <- target_var_Loglik_fun(away_Home_SP_bullpen_Mod_vars, home_Away_Batting_Rolling_mod_Vars, target_Var)
data


##-- select mod var distrn
home_Away_Batting_Rolling_mod_Vars %>%
  select(game_pk, game_date, cum_Obs, total_Game_runs,
         away_Batting_rolling_HA_BB_Outs_Int_values_EV_Per_9:triple_Park_effects) -> home_Away_Batting_Rolling_mod_Vars_sub


##-- left complete dt for all 3 stat mods 
away_Home_SP_bullpen_Mod_vars %>%
  right_join(home_Away_Batting_Rolling_mod_Vars_sub, by = c("game_pk","game_date")) %>%
  mutate(target_Var = if_else(total_Game_runs > target_Var, 1L, 0L)) %>% # this target var has to be adjust to calc loglik **
  filter(!(is.na(away_Bullpen_rolling_HA_Homeruns_per_9))) %>%
  filter(!(is.na(home_Bullpen_rolling_HA_Homeruns_per_9))) -> away_Home_SP_bullpen_Mod_vars



##-- add ordinal res var for O/U 6.5, 9.5, 12.5, 14.5
# away_Home_SP_bullpen_Mod_vars %>%
#   mutate(ordinal_Target_var = case_when(total_Game_runs < 4 ~ "1-3",
#                                         total_Game_runs >= 4 & total_Game_runs < 7 ~ "4-6",
#                                         total_Game_runs >= 7 & total_Game_runs < 10 ~ "7-9",
#                                         total_Game_runs >= 10 & total_Game_runs < 13 ~ "10-12",
#                                         total_Game_runs >= 12 & total_Game_runs < 15 ~ "12-14",
#                                         total_Game_runs >= 15 ~ "15+")) %>%
#   mutate(ordinal_Target_var = factor(ordinal_Target_var, levels = c("1-3", "4-6", "7-9",
#                                                                     "10-12", "12-14", "15+" ), ordered = TRUE)) -> away_Home_SP_bullpen_Mod_vars

# # total O/U 7.5, 10.5, 13.5, 15.5
away_Home_SP_bullpen_Mod_vars %>%
  mutate(ordinal_Target_var = case_when(total_Game_runs < 5 ~ "1-4",
                                        total_Game_runs >= 5 & total_Game_runs < 8 ~ "5-7",
                                        total_Game_runs >= 8 & total_Game_runs < 11 ~ "8-10",
                                        total_Game_runs >= 11 & total_Game_runs < 14 ~ "11-13",
                                        total_Game_runs >= 13 & total_Game_runs < 16 ~ "13-15",
                                        total_Game_runs >= 16 ~ "16+")) %>%
  mutate(ordinal_Target_var = factor(ordinal_Target_var, levels = c("1-4", "5-7", "8-10",
                                                                    "11-13", "13-15", "16+" ), ordered = TRUE)) -> away_Home_SP_bullpen_Mod_vars



# # 
# #  ## total O/U 8.5, 11.5, 14.5
 # away_Home_SP_bullpen_Mod_vars %>%
 #   mutate(ordinal_Target_var = case_when(total_Game_runs < 4 ~ "1-3",
 #                                         total_Game_runs >= 4 & total_Game_runs < 7 ~ "4-6",
 #                                         total_Game_runs >= 7 & total_Game_runs < 9 ~ "7-8",
 #                                         total_Game_runs >= 9 & total_Game_runs < 12 ~ "9-11",
 #                                         total_Game_runs >= 12 & total_Game_runs < 15 ~ "12-14",
 #                                         total_Game_runs >= 15 ~ "15+")) %>%
 #   mutate(ordinal_Target_var = factor(ordinal_Target_var, levels = c("1-3", "4-6", "7-8",
 #                                                                      "9-11", "12-14", "15+" ), ordered = TRUE)) -> away_Home_SP_bullpen_Mod_vars

# # #  
# 
#  ## total O/U 5.5
#  away_Home_SP_bullpen_Mod_vars %>%
#    mutate(ordinal_Target_var = case_when(total_Game_runs < 4 ~ "1-3",
#                                          total_Game_runs >= 4 & total_Game_runs < 6 ~ "4-5",
#                                          total_Game_runs >= 6 & total_Game_runs < 9 ~ "6-8",
#                                          total_Game_runs >= 9 & total_Game_runs < 12 ~ "9-11",
#                                          total_Game_runs >= 12 & total_Game_runs < 15 ~ "12-14",
#                                          total_Game_runs >= 15 ~ "15+")) %>%
#    mutate(ordinal_Target_var = factor(ordinal_Target_var, levels = c("1-3", "4-5", "6-8",
#                                                                      "9-11", "12-14", "15+" ), ordered = TRUE)) -> away_Home_SP_bullpen_Mod_vars
# 



## sum the prob of binned cate going over 
## binary equivalent of tar == 1
fit_polr_mod <- function(mod, df) 
{
  
  pred_polr <-  predict(mod, df, type = "p")
  
  as.data.frame(pred_polr) %>%
    mutate(prob_Under = (`1-4` + `5-7`)) %>%
    mutate(prob_Over = 1 - prob_Under) %>%
    select(prob_Over)
  
}


##=-- kfolds spliuts data into smaller and smaller groups whilst crossv_mc split data into 2 groups reps "x" times
away_Home_SP_bullpen_Mod_vars %>%
  crossv_mc(20, test = 0.5) -> GameDay_Cv


##-- clm mod 
GameDay_Cv %>% 
  mutate(train = map(train, as_tibble)) %>%
  mutate(test = map(test, as_tibble)) %>%
  mutate(clm_Mod_bat = map(train,  ~MASS::polr(ordinal_Target_var ~  I(ns(cum_Obs,2)*10) + 
                                                 (double_Park_effects + triple_Park_effects + hr_Park_effects + single_Park_effects) + +
                                                 
                                                 (home_SP_rolling_HA_Strikeouts_per_9 + home_SP_rolling_HA_BB_Outs_Int_values_EV_Per_9 + home_SP_rolling_HA_Homeruns_per_9 +
                                                    home_SP_rolling_HA_off_Int_values_Weighted_EV_per_9):home_SP_innings_Pitched_int + 
                                                 
                                                 (away_SP_rolling_HA_Strikeouts_per_9 + away_SP_rolling_HA_BB_Outs_Int_values_EV_Per_9 + away_SP_rolling_HA_Homeruns_per_9 +
                                                    away_SP_rolling_HA_off_Int_values_Weighted_EV_per_9):away_SP_innings_Pitched_int + 
                                                 
                                                 (home_Bullpen_rolling_HA_Homeruns_per_9 + home_Bullpen_rolling_HA_off_Int_values_Weighted_EV_per_9):home_Bullpen_innings_Pitched_int +  
                                                 (away_Bullpen_rolling_HA_Homeruns_per_9 + away_Bullpen_rolling_HA_off_Int_values_Weighted_EV_per_9):away_Bullpen_innings_Pitched_int ,
                                               data = .,))) %>%
  mutate(clm_Mod_bat = map2(clm_Mod_bat, test, fit_polr_mod)) %>% # probs Over target
  unnest(clm_Mod_bat, test) %>%
  select(target_Var, prob_Over)  %>%
  mutate(mod_logLik = if_else(target_Var == 0L, -log(1 -prob_Over ), -log(prob_Over ))) %>% # loglik calc
  group_by(target_Var) %>%
  summarise(lower = quantile(mod_logLik,.20, na.rm = TRUE),
            med = median(mod_logLik, na.rm = TRUE),
            upper= quantile(mod_logLik,.80, na.rm = TRUE),
            Cnt = sum(n()))  -> Pitching_Loglik


##-- clm mod 
GameDay_Cv %>% 
  mutate(train = map(train, as_tibble)) %>%
  mutate(test = map(test, as_tibble)) %>%
  mutate(clm_Mod_bat = map(train,  ~MASS::polr(ordinal_Target_var ~  I(ns(cum_Obs,2)*10) + 
                                                 (double_Park_effects + triple_Park_effects + hr_Park_effects + single_Park_effects) +
                                                 
                                                 (away_Batting_rolling_HA_Strikeouts_per_9 +  away_Batting_rolling_HA_off_Int_values_Weighted_EV_per_9 + 
                                                    away_Batting_rolling_HA_BB_Outs_Int_values_EV_Per_9 + away_Batting_rolling_HA_Homeruns_per_9):home_SP_innings_Pitched_int +
                                                 
                                                 (home_Batting_rolling_HA_Strikeouts_per_9 + home_Batting_rolling_HA_off_Int_values_Weighted_EV_per_9 +
                                                    home_Batting_rolling_HA_BB_Outs_Int_values_EV_Per_9 + home_Batting_rolling_HA_Homeruns_per_9):away_SP_innings_Pitched_int,
                                               data = .,))) %>%
  mutate(clm_Mod_bat = map2(clm_Mod_bat, test, fit_polr_mod)) %>% # probs Over target
  unnest(clm_Mod_bat, test) %>%
  select(target_Var, prob_Over)  %>%
  mutate(mod_logLik = if_else(target_Var == 0L, -log(1 -prob_Over ), -log(prob_Over ))) %>% # loglik calc
  group_by(target_Var) %>%
  summarise(lower = quantile(mod_logLik,.20, na.rm = TRUE),
            med = median(mod_logLik, na.rm = TRUE),
            upper= quantile(mod_logLik,.80, na.rm = TRUE),
            Cnt = sum(n()))  -> Bat_Loglik


##-- clm mod 
GameDay_Cv %>% 
  mutate(train = map(train, as_tibble)) %>%
  mutate(test = map(test, as_tibble)) %>%
  mutate(clm_Mod_bat = map(train,  ~MASS::polr(ordinal_Target_var ~  I(ns(cum_Obs,2)*10) +
                                                 (double_Park_effects + triple_Park_effects + hr_Park_effects + single_Park_effects) + 
                                                 
                                                 (home_SP_rolling_HA_Strikeouts_per_9:away_Batting_rolling_HA_Strikeouts_per_9 + 
                                                    home_SP_rolling_HA_off_Int_values_Weighted_EV_per_9:away_Batting_rolling_HA_off_Int_values_Weighted_EV_per_9 +
                                                    home_SP_rolling_HA_Homeruns_per_9:away_Batting_rolling_HA_Homeruns_per_9 +
                                                    home_SP_rolling_HA_BB_Outs_Int_values_EV_Per_9:away_Batting_rolling_HA_BB_Outs_Int_values_EV_Per_9):home_SP_innings_Pitched_int +
                                                 
                                                 (away_SP_rolling_HA_Strikeouts_per_9:home_Batting_rolling_HA_Strikeouts_per_9 + 
                                                    away_SP_rolling_HA_off_Int_values_Weighted_EV_per_9:home_Batting_rolling_HA_off_Int_values_Weighted_EV_per_9 +
                                                    away_SP_rolling_HA_Homeruns_per_9:home_Batting_rolling_HA_Homeruns_per_9 +
                                                    away_SP_rolling_HA_BB_Outs_Int_values_EV_Per_9:home_Batting_rolling_HA_BB_Outs_Int_values_EV_Per_9):away_SP_innings_Pitched_int +
                                                 
                                                 (home_Bullpen_rolling_HA_off_Int_values_Weighted_EV_per_9:away_Batting_rolling_HA_off_Int_values_Weighted_EV_per_9 +
                                                    home_Bullpen_rolling_HA_Homeruns_per_9:away_Batting_rolling_HA_Homeruns_per_9):home_Bullpen_innings_Pitched_int +
                                                 
                                                 ( away_Bullpen_rolling_HA_off_Int_values_Weighted_EV_per_9:home_Batting_rolling_HA_off_Int_values_Weighted_EV_per_9 +
                                                     away_Bullpen_rolling_HA_Homeruns_per_9:home_Batting_rolling_HA_Homeruns_per_9):away_Bullpen_innings_Pitched_int ,
                                               data = .,))) %>%
  mutate(clm_Mod_bat = map2(clm_Mod_bat, test, fit_polr_mod)) %>% # probs Over target
  unnest(clm_Mod_bat, test) %>%
  select(target_Var, prob_Over)  %>%
  mutate(mod_logLik = if_else(target_Var == 0L, -log(1 -prob_Over ), -log(prob_Over ))) %>% # loglik calc
  group_by(target_Var) %>%
  summarise(lower = quantile(mod_logLik,.20, na.rm = TRUE),
            med = median(mod_logLik, na.rm = TRUE),
            upper= quantile(mod_logLik,.80, na.rm = TRUE),
            Cnt = sum(n()))  -> Bat_Ball_Loglik

Pitching_Loglik
Bat_Loglik
Bat_Ball_Loglik

