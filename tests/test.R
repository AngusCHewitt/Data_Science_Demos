library(tidyverse)
library(splines) # natrtual spline NL relationship
#\library(sjPlot) #mixed effects plots
library(lme4) # mixed models 
library(plotly)
library(modelr)
library(ggeffects)

load("data/fitted_Mixed_mod.rds") # liver mixed model 
load("data/liver_Log_dt.rds") # liver mixed model 
load("data/liver_dt.rds") # liver mixed model 

glimpse(liver_Log_dt)
summary(fitted_Mixed_mod)

summary(liver_Log_dt$Direct_Bilirubin)


mod_Effects_dt <- ggpredict(fitted_Mixed_mod)

ggplotly(plot(mod_Effects_dt$Direct_Bilirubin))


## ggplot of prob errorbars 
combined_Pred_sample_DT %>%
  ggplot(aes(x = Age, y = Prediction, fill = Gender)) + geom_line() + 
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = .2) + 
  facet_wrap(~Gender) +
  theme_classic() + theme(legend.position = "none") + labs(x = "", y = "Probability of liver Disease (%)") -> p
  
## interactive viz 
ggplotly(p)





          