library(caret)
## Loading required package: lattice
## Loading required package: ggplot2
str(GermanCredit)

set.seed(1984)

training <- createDataPartition(GermanCredit$Class, p = 0.6, list=FALSE)

trainData <- GermanCredit[training,]
testData <- GermanCredit[-training,]

glmModel <- glm(Class~ Age + Amount + InstallmentRatePercentage + ResidenceDuration  , data=trainData, family=binomial)

pred.glmModel <- predict(glmModel, newdata=testData, type="response")

# predict singular no.
predict(glmModel, newdata = data.frame(Age = 50, Amount = 10000, InstallmentRatePercentage= 3, ResidenceDuration = 3), type="response")

library(pROC)

roc.glmModel <- pROC::roc(testData$Class, pred.glmModel)
plot(roc.glmModel)

auc.glmModel <- pROC::auc(roc.glmModel)


# model hyper parameters
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

set.seed(2014)


# fit caret model - dependant on 2 pcks
glmBoostModel <- train(Class ~  . , data=trainData, 
                       method = "glmboost", metric="ROC", trControl = fitControl, tuneLength=5, center=TRUE, 
                       family=Binomial(link = c("logit")))

# prob of being a good credit
pred.glmBoostModel <- as.vector(predict(glmBoostModel, newdata=testData, type="prob")[,"Good"])

# model accuracy
roc.glmBoostModel <- pROC::roc(testData$Class, pred.glmBoostModel)

auc.glmBoostModel <- pROC::auc(roc.glmBoostModel)

##-- alt methods

# CART
set.seed(2014)

# fit CART model
cartModel <- train(Class ~ ., data=trainData, method = "rpart", metric="ROC", trControl = fitControl, tuneLength=5)

names(cartModel)

# predict credit scores 
pred.cartModel <- as.vector(predict(cartModel, newdata=testData, type="prob")[,"Good"])

# test model acc
roc.cartModel <- pROC::roc(testData$Class, pred.cartModel)

auc.cartModel <- pROC::auc(roc.cartModel)

