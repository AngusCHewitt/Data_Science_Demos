##-- testing sampling distrn ability to predict or represent out of sample results --##
library(tidyverse)
library(lme4)
library(lmerTest)
library(broom)
library(gidExtra)
library(RcmdrMisc)
library(vcd)

# load dataset
setwd("/Users/angushewitt/Desktop/AFL Datasets/")
#setwd("//internal.vic.gov.au/DHHS/HomeDirs6/ahew2812/Documents/Downloads/AFL Datasets/")

# use 25% of data to cross validate the model 
load("AFL_Tabs_Model_DS.Rdata")

# add unqiue row ID
AFL_tabs_dt %>%
mutate(ID_number = 1:nrow(AFL_tabs_dt)) -> AFL_tabs_dt

# sample 25% of data
 set.seed(123)
 AFL_tabs_dt %>%
 group_by(game_ID) %>%
 nest() -> nest_game_level
 
 # keep 44 player of each game in the cross validated samples
   nest_game_level %>%
   sample_n(size = nrow(nest_game_level) * .75) %>%
   unnest() %>%
   dplyr::select(ID, game_ID,ID_number, Career_GDRole_Cat, PCA_GS_Form, PCA_GS_Potential, Age_Categories, 
           Height_Categories, HGS_Binary) -> Training_Data

# edit not in data object function
'%!in%' <- function(x,y)!('%in%'(x,y))

##-- test dataset
AFL_tabs_dt %>%
  dplyr::filter(game_ID %!in% Training_Data$game_ID) %>%
  dplyr::select(ID, game_ID,ID_number, Career_GDRole_Cat, PCA_GS_Form, PCA_GS_Potential, Age_Categories, 
                Height_Categories, HGS_Binary) -> Test_Dataset

# rehsape test dataset to allow for comparision with a preiction mod
Test_Dataset %>%
  group_by(Career_GDRole_Cat, HGS_Binary) %>%
  summarise(total_count = n()) %>%
  spread(key = HGS_Binary, value = total_count, fill = 0) %>%
  mutate(prob_success = `1`/sum(`1`,`0`)) -> reshaped_Test_dt

##-- baseline mod
glm <- glm(HGS_Binary ~ Career_GDRole_Cat + PCA_GS_Potential + PCA_GS_Form + Age_Categories + Height_Categories, data =  Training_Data,
           family = binomial(logit))  

summary(glm)
base_glm_prediction <- predict(glm, newdata = Test_Dataset, type = "response")

##-- join prediction to summarised dataset
Test_Dataset %>% 
  ungroup() %>%
  mutate(predictions = base_glm_prediction) %>%
  group_by(Career_GDRole_Cat) %>%
  summarise(pred_mean  = mean(predictions))  -> glm_prediction

# summary mod errors
summary(glm_prediction$pred_mean - test$prob_success)

  
##-- glmer - Baseline mixed model
base_mixed_effects <- glmer(HGS_Binary ~ Height_Categories + Age_Categories + PCA_GS_Potential + PCA_GS_Form + (1|Career_GDRole_Cat) ,
                      data = Training_Data, family = binomial(link = "logit"), optimizer = "bobyqa")

summary(base_mixed_effects)
base_mixed.mod_prediction <- predict(base_mixed_effects, newdata = Test_Dataset, type = "response")

##-- join prediction to summarised dataset
Test_Dataset %>% 
  ungroup() %>%
  mutate(predictions = base_glm_prediction) %>%
  group_by(Career_GDRole_Cat) %>%
  summarise(pred_mean  = mean(predictions))  -> base_mixed.mod_prediction

# summary mod errors
summary(base_mixed.mod_prediction$pred_mean - test$prob_success)


##-- glmer - Gameday form Rand Effect Fixed Intercept Career/GD_Role
mixed_effects_1 <- glmer(HGS_Binary ~ Height_Categories + Age_Categories + PCA_GS_Potential + PCA_GS_Form + (PCA_GS_Form|Career_GDRole_Cat) ,
                         data = Training_Data, family = binomial(link = "logit"), optimizer = "bobyqa")

summary(mixed_effects_1)
mixed_effects_1_prediction <- predict(mixed_effects_1, newdata = Test_Dataset, type = "response")

##-- join prediction to summarised dataset
Test_Dataset %>% 
  ungroup() %>%
  mutate(predictions = base_glm_prediction) %>%
  group_by(Career_GDRole_Cat) %>%
  summarise(pred_mean  = mean(predictions))  -> mixed_effects_1_prediction

# summary mod errors
summary(mixed_effects_1_prediction$pred_mean - test$prob_success)
summary(out_sample_base_mixed.mod_prediction$pred_mean - test$prob_success)
summary(out_sample_mixed.mod_1_prediction$pred_mean - test$prob_success)

# visualise mod errors
boxplot(out_sample_base_glm_prediction$pred_mean - test$prob_success)
boxplot(out_sample_base_mixed.mod_prediction$pred_mean - test$prob_success)
boxplot(out_sample_mixed.mod_1_prediction$pred_mean - test$prob_success)


##-- Old Code --##

# Baseline %'s used
Training_Data %>%
  group_by(Career_GDRole_Cat,  HGS_Binary) %>%
  summarise(total_count = n()) %>%
  spread(key = HGS_Binary, value = total_count, fill = 0) %>%
  mutate(prob_success = `1`/sum(`1`,`0`)) -> train_simulation


# Sample with replacement
set.seed(123)
Training_Data %>%
  ungroup() %>%
  sample_n(size = 1000000, replace = T) -> Sample_Training_Dataset


test_sample <- rbinom(n = 1000, size = 2000, prob = .0864)


summary(test_sample/2000)
