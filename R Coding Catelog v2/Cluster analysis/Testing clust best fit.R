##-- Cluster model godness of bit test
# mclust to id player career stats from 2000-19 

library(tidyverse)
library(flexmix) # poisson mix mods]\
library(RcmdrMisc)

setwd("/Users/angushewitt/Desktop/AFL Datasets/Pre 2020 Data/")

# Careers features dataset
load("Step 3_AFL_lagged_TS_stats_dt.RData")


##-- repalce NA'S with career category "first game" 
AFL_lagged_TS_stats_dt %>%
  dplyr::filter(Season >= 2004) %>%
  mutate(Career_Position = (if_else(is.na(lagged_Goals),"First_Gamer","Other"))) -> AFL_Career_Positions



##-- coerce model vars into imteger
AFL_Career_Positions %>%
  ungroup() %>%
  mutate(obs = 1:nrow(AFL_Career_Positions)) %>%
  gather("Var","Value",TS_Running_Med_Bounces, TS_Running_Med_Clearances,
         TS_Running_Med_Hit.Outs, TS_Running_Med_Inside.50s, TS_Running_Med_Marks_Out50,
         TS_Running_Med_Marks.Inside.50, TS_Running_Med_One.Percenters, TS_Running_Med_Rebounds,
         TS_Running_Med_SOG, TS_Running_Range_Bounces, TS_Running_Range_Clearances,
         TS_Running_Range_Hit.Outs, TS_Running_Range_Inside.50s, TS_Running_Range_Marks_Out50,
         TS_Running_Range_Marks.Inside.50, TS_Running_Range_One.Percenters, TS_Running_Range_Rebounds,
         TS_Running_Range_SOG)  %>%
  mutate(Value = round(Value,2)) %>%
  spread(key = Var, value = Value) %>%
  dplyr::select(-obs) -> AFL_Career_Positions


##-- filter out first games from the mixture model (have top stack on later)
AFL_Career_Positions %>%
  dplyr::filter(Career_Position == "First_Gamer") -> First_Gamers

##-- AFL tabs minus first gamers
AFL_Career_Positions %>% 
  dplyr::filter(Career_Position != "First_Gamer") -> AFL_Career_Positions

set.seed(1001)
Career_Sample <- sample_n(AFL_Career_Positions, 10000, replace = FALSE)

Career_Sample %>%
  dplyr::select(TS_Running_Med_Bounces, TS_Running_Med_Clearances,
                TS_Running_Med_Hit.Outs, TS_Running_Med_Inside.50s, TS_Running_Med_Marks_Out50,
                TS_Running_Med_Marks.Inside.50, TS_Running_Med_One.Percenters, TS_Running_Med_Rebounds,
                TS_Running_Med_SOG, TS_Running_Range_Bounces, TS_Running_Range_Clearances,
                TS_Running_Range_Hit.Outs, TS_Running_Range_Inside.50s, TS_Running_Range_Marks_Out50,
                TS_Running_Range_Marks.Inside.50, TS_Running_Range_One.Percenters, TS_Running_Range_Rebounds,
                TS_Running_Range_SOG) -> gameday_stats


dt_matrix <- as.matrix(gameday_stats)


# Set k to 15 for career position and Gameday produce consistent compariable peer groups 
set.seed(1008)
mvpois_mix_model <- stepFlexmix(dt_matrix ~ 1, 
                                k = 10:20, 
                                nrep = 10, 
                                model = FLXMCmvpois(),
                                control = list(iter.max = 1000, tolerance = 0.001, classify = "weighted"))

# find the best fitted model according to the lowest BIC value
best_fit4 <- getModel(mvpois_mix_model, which = "AIC")


param <- data.frame(parameters(best_fit1))


param <- param %>% mutate(Type = colnames(gameday_stats))

# reshape dataset to component to join lambda parameters   
AFL_Career_Positions %>%
  select(ID,Date, TS_Running_Med_Bounces, TS_Running_Med_Clearances,
         TS_Running_Med_Hit.Outs, TS_Running_Med_Inside.50s, TS_Running_Med_Marks_Out50,
         TS_Running_Med_Marks.Inside.50, TS_Running_Med_One.Percenters, TS_Running_Med_Rebounds,
         TS_Running_Med_SOG, TS_Running_Range_Bounces, TS_Running_Range_Clearances,
         TS_Running_Range_Hit.Outs, TS_Running_Range_Inside.50s, TS_Running_Range_Marks_Out50,
         TS_Running_Range_Marks.Inside.50, TS_Running_Range_One.Percenters, TS_Running_Range_Rebounds,
         TS_Running_Range_SOG) %>%
  mutate(Cluster_Components = clusters(best_fit1)) %>%
  gather("Variables","Values",-ID,-Date,-Cluster_Components) -> reshaped_Career_stats


# joined compoents paramters to each variable used in mod1
reshaped_Career_stats %>%
  left_join(param, by = c("Variables" = "Type")) -> reshaped_Career_stats

# Calc the goodness of fit for each campus/component/Var - combos
reshaped_Career_stats %>%
  mutate(sum_stand_diffs = case_when(Cluster_Components == 1 ~ ((Values + 1) - (Comp.1 + 1))/(Values + 1),
                                     Cluster_Components == 2 ~ ((Values + 1) - (Comp.2 + 1))/(Values + 1),
                                     Cluster_Components == 3 ~ ((Values + 1) - (Comp.3 + 1))/(Values + 1),
                                     Cluster_Components == 4 ~ ((Values + 1) - (Comp.4 + 1))/(Values + 1),
                                     Cluster_Components == 5 ~ ((Values + 1) - (Comp.5 + 1))/(Values + 1),
                                     Cluster_Components == 6 ~ ((Values + 1) - (Comp.6 + 1))/(Values + 1),
                                     Cluster_Components == 7 ~ ((Values + 1) - (Comp.7 + 1))/(Values + 1),
                                     Cluster_Components == 8 ~ ((Values + 1) - (Comp.8 + 1))/(Values + 1),
                                     Cluster_Components == 9 ~ ((Values + 1) - (Comp.9 + 1))/(Values + 1),
                                     Cluster_Components == 10 ~ ((Values + 1) - (Comp.10 + 1))/(Values + 1)))  -> reshaped_Career_stats

# sum of abs differences
numSummary(reshaped_Career_stats[, "sum_stand_diffs", drop = FALSE], 
           statistics = c("mean", "sd", "IQR", "quantiles"), quantiles = c(0, 0.25, 0.5, 0.75, 1))


