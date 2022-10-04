##-- ML mod - Ranger --##

# Adapted from the caret vignette
library(tidyverse)
library("caret")
library("mlbench")
library("pROC")
library("rpart")
library("caretEnsemble")
library("randomForest")
library("nnet")
library("arm")
library("brnn")
library("ranger")
library("caTools")


setwd("/Users/angushewitt/Desktop/AFL Datasets/")

# load training datasets
load("training_dt_Round_1_2019.Rdata")
load("training_dt_Round_12_2019.Rdata")
load("training_dt_Round_21_2019.Rdata")

# load test datasets
load("test_dt_Round_1_2019.Rdata")
load("test_dt_Round_12_2019.Rdata")
load("test_dt_Round_21_2019.Rdata")


# model controk parameters
set.seed(107)
# tuning parameters
my_control <- trainControl(
  method="repeatedcv", 
  number=10, 
  repeats=10,
  classProbs = TRUE,
  search= "random",
  sampling = "down",
  summaryFunction=twoClassSummary,
  savePredictions = "all")

# custom tuning grid
tgrid <- expand.grid(
  .mtry = 11,
  .splitrule = "gini",
  .min.node.size = 20)

# Baseline full model with all relevant and meaningful vars added
model_list <- train( Highest_Goal_Scorer  ~ Career_Position + Height_Categories + GameDay_Role  + rain_yes_or_no +  
                       Career_Range_Marks.Inside.50 + Career_Max_Inside.50s + 
                       + Team_Points_For_MA_5 + Team_Points_Against_MA_5  + 
                       Career_Max_Goals + Career_Mark_Inside_50_PG + Career_Range_Goals  + Age_Categories + 
                       Career_Goals_PG  + Home_Game + Goals_Form + Mark_Inside_50_Form, 
  data= training_dt_Round_12_2019,
  metric="ROC",
  tuneGrid = tgrid,
  method="ranger",
  trControl=my_control,
  importance = "permutation")

# test var importance
rangerImp <- varImp(model_list, scale = FALSE)
plot(rangerImp)

model_list$finalModel$importance.mode

# Predict on test: p
p <- predict(model_list, newdata = test_dt_Round_12_2019, type = "prob")

# Make ROC curve
colAUC(p , test_dt_Round_12_2019[["Highest_Goal_Scorer"]], plotROC = TRUE)

##-- add prob to test dataset
test_dt_Round_12_2019 %>%
  mutate(id = 1:nrow(test_dt_Round_12_2019)) -> test_dt_Round_12_2019

p %>%
 mutate(id = 1:nrow(test_dt_Round_12_2019)) -> p


## join p dataset to test ds
test_dt_Round_12_2019 %>%
  left_join(p, by = "id") -> test_dt_Round_12_2019

test_dt_Round_12_2019 %>%
  dplyr::select(First.name, Surname, HGS, NotHGS, Career_GDRole_Cat, Highest_Goal_Scorer) %>%
  View()

##-- Old Code --##

# sample training dataset
set.seed(161)
training_dt_Round_1_2019 %>%
  ungroup() %>%
  sample_n(size = 10000,replace = TRUE) -> AFL_Sample
