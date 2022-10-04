##-- ML mod - rpart --##

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



# Baseline full model with all relevant and meaningful vars added
model_list <- train(Highest_Goal_Scorer  ~ Career_GDRole_Cat * Height_Categories + rain_gr_6mm +  
                       Career_Range_Marks.Inside.50 + Career_Max_Inside.50s + 
                       + Team_Points_For_MA_5 + Team_Points_Against_MA_5  + 
                       Career_Max_Goals + Career_Mark_Inside_50_PG + Career_Range_Goals  + Age_Categories + 
                       Career_Goals_PG  + Home_Game + Goals_Form + Mark_Inside_50_Form, 
                     data= training_dt_Round_12_2019,
                     metric="ROC",
                     method="rpart",
                     trControl=my_control,
                     preProcess = c("center","scale"))

# test var importance
rpartImp <- varImp(model_list, scale = FALSE)
plot(rpartImp)


# Predict on test: p
p <- predict(model_list, newdata = test_dt_Round_12_2019, type = "prob")

# Make ROC curve
colAUC(p , test_dt_Round_12_2019[["Highest_Goal_Scorer"]], plotROC = TRUE)

help.search("colAUC")


##-- old code --##

# sample training dataset
set.seed(161)
training_dt_Round_1_2019 %>%
  ungroup() %>%
  sample_n(size = 10000, replace = F) -> AFL_Sample


modelLookup("rpart")

help.search("rpart")

?rpart
