##-- example of caret Ensemble -##
##-- weighted combiation of mods - optimaal level of xyz error metric --##

#Adapted from the caret vignette
library("caret")
library("mlbench")
library("pROC")
library("rpart")
library("caretEnsemble")
library("randomForest")
library("nnet")

data(Sonar)
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTrain,]
testing <- Sonar[-inTrain,]

# tuning parameters
my_control <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",
  classProbs=TRUE,
  index=createResample(training$Class, 25),
  summaryFunction=twoClassSummary
)

# list model to pass through caret Ensemble
model_list <- caretList(
  Class~., data=training,
  trControl=my_control,
  methodList=c("glm", "rpart")
)

# print mod prediction 
p <- as.data.frame(predict(model_list, newdata=head(testing)))
print(p)

# example of passing in tuning parameters
model_list_big <- caretList(
  Class~., data=training,
  trControl=my_control,
  metric="ROC",
  methodList=c("glm", "rpart"),
  tuneList=list(
    rf1=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=2)),
    rf2=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=10), preProcess="pca"),
    nn=caretModelSpec(method="nnet", tuneLength=2, trace=FALSE)
  )
)



# visualse mod performance
xyplot(resamples(model_list))


# test model correlations
modelCor(resamples(model_list))

# as the mod are uncorrelated they are good candiates for ensemble
greedy_ensemble <- caretEnsemble(
  model_list, 
  metric="ROC",
  trControl=trainControl(
    number=2,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
  ))

## The following models were ensembled: glm, rpart 
## They were weighted: 
## 1.4489 -0.9559 -2.0442
## The resulting ROC is: 0.7573
## The fit for each individual model on the ROC is: 
##  method       ROC      ROCSD
##     glm 0.6829333 0.05890797
##   rpart 0.7206765 0.06849524

# Combination method is about 7% than any of the individual mods
summary(greedy_ensemble)
