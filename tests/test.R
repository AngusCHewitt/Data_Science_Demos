library(tidyverse)
library(splines) # natrtual spline NL relationship
#\library(sjPlot) #mixed effects plots
library(lme4) # mixed models 
library(plotly)
library(modelr)


load("data/fitted_Mixed_mod.rds") # liver mixed model 
load("data/liver_Log_dt.rds") # liver mixed model 


## creae model input df 0- newdata 
data.frame(Gender = "F",
           Age = 45, # demos
           Direct_Bilirubin = log(2 + 1), # medical tests
           Alkaline_Phosphotase = log(3 + 1),
           Alamine_Aminotransferase = log(5 + 1),
           Aspartate_Aminotransferase = log(7 + 1)) -> pred_DF

## test model effects 
sample <- liver_Log_dt[2:5,] %>%
  select(Gender, Age, Direct_Bilirubin, Alkaline_Phosphotase, Alamine_Aminotransferase, Aspartate_Aminotransferase)
combined_Newdata_sample <- rbind(pred_DF, sample)


## test model effects 
data <- liver_Log_dt[1,] 
sample <- liver_Log_dt[2:5,]
combined_Newdata_sample <- rbind(data, sample)

## need atleast 5 obs to generate se.fit, take the first obs resultsm viz an eeorbar , max / min (0,1) 
pred <- predict(fitted_Mixed_mod, newdata = combined_Newdata_sample, type = "response", se.fit = TRUE) # add standard errors 

## pred dataset, likelihood & =- standard errors 
data.frame(pred) %>%
  mutate(lower_bound = fit - se.fit) %>%
  mutate(upper_bound = fit + se.fit) %>%
  mutate(lower_bound = if_else(lower_bound < 0, 0, lower_bound)) %>%
  mutate(upper_bound = if_else(upper_bound > 1, 1, upper_bound)) %>%
  mutate(lower_bound = round(lower_bound,2)*100) %>%
  mutate(upper_bound = round(upper_bound,2)*100) %>%
  mutate(fit = round(fit,2)*100) %>%
  mutate(label = "probablity of liver disease") -> prediction_data

## ggplot of prob errorbars 
prediction_data[1,] %>%
  ggplot(aes(x = label, y = fit)) + geom_point(size = 4, alpha = .3, colour ="red") + 
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound)) + 
  geom_point(aes(y = lower_bound)) + geom_point(aes(y = upper_bound)) + 
  theme_classic() + labs(x = "", y = "Probability (%)") -> p
  
## interactive viz 
ggplotly(p)





          