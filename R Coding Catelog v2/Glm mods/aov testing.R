
##-- fit goals scoring models to lagged data --##
## From Venables and Ripley (2002) p.165.
library(stats)
library(tidyverse) # tidy data
library(lme4) #mixed mods
library(modelr) # many mods
library(broom.mixed) # tidy model vars
library(effects) # visualise models effects
library(statmod)# mod stats 
library(ggeffects) # plot mixd effects
library(splines) # natrtual spline NL relationship
library(sjPlot) #mixed effects plots

##-- load lagged afl tabs
load("~/Desktop/Odds Machine/AFL App Dev/Datasets/Historical/Step_4_pred_Features_2013_2021_lagged.rds")



##-- filter to rolling psoition Forward (**)
afl.com_Adv_stats_2012_2021_lagged %>%
  filter(rolling_Positions == "Forward") -> Forward_dt

#
##--anytime mixed mod
anytime_mixed_mod_Forward <-  glm(anytime_Goals_binary ~ TOG_cate_No + Frwd_Positions_fantasy_Ranking_within_Team +  Rolling_ewma_playing.for_scores:Rolling_ewma_against_scores   + 
                                      Rolling_lower_Fantasy_goal_Scoring_stats + Rolling_med_Fantasy_goal_Scoring_stats + Rolling_upper_Fantasy_goal_Scoring_stats +
                                      Rolling_med_Def_Stats:Rolling_lower_Def_Stats:Rolling_upper_Def_Stats     ,
                                    data = Forward_dt, family = binomial(link = "logit"))


##-- the order of the model does matter in term of the first order var have much higher sum of square, interaction term auto after univaariant terms
## can only feed in a glm here so have to see how thats going work for gam model in the ED?AV work
anytime_mixed_mod_Forward_alt <-  glm(anytime_Goals_binary ~ TOG_cate_No,
                                  data = Forward_dt, family = binomial(link = "logit"))

summary(aov(anytime_mixed_mod_Forward))
anova(anytime_mixed_mod_Forward)


##-- mod vafrs
Forward_dt %>%
  select(anytime_Goals_binary, TOG_cate_No , Frwd_Positions_fantasy_Ranking_within_Team ,  Rolling_ewma_playing.for_scores, Rolling_ewma_against_scores   , 
           Rolling_lower_Fantasy_goal_Scoring_stats , Rolling_med_Fantasy_goal_Scoring_stats , Rolling_upper_Fantasy_goal_Scoring_stats ,
           Rolling_med_Def_Stats:Rolling_lower_Def_Stats:Rolling_upper_Def_Stats)  -> mod_vars


mod_vars[-1,]



##-------------------------------- Examples ------------------------------------##
## Set orthogonal contrasts.
library(stats)
## to show the effects of re-ordering terms contrast the two fits
mod <- lm(yield ~ block + N * P + K, npk)
anova(mod)

##-- % variance in y which can be explained by variations in x 
anova_fit <- anova(mod)

