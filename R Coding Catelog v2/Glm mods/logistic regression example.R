# import r stats tute dataset
inputData <- read.csv("http://rstatistics.net/wp-content/uploads/2015/09/adult.csv")

str(inputData)

# create a table with the counts of workers earning >50K and less than 50K
x <- table(inputData$ABOVE50K)

# return cells % for the binary category
prop(x)



# Create Training Data
input_ones <- inputData[which(inputData$ABOVE50K == 1), ]  # all 1's
input_zeros <- inputData[which(inputData$ABOVE50K == 0), ]  # all 0's
set.seed(100)  # set seed to create a reproduceable sample

## give a selection of row numbers from both the above datasets with equal distribution of 0 & 1's

input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ] #select sampled rows for dataset for all 1's  
training_zeros <- input_zeros[input_zeros_training_rows, ] #select sampled rows for dataset for all 0's
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

# Create Test Data
# Validation dataset created by excluding training rows

test_ones <- input_ones[-input_ones_training_rows, ] 
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 

library(smbinning)
# segregate continuous and factor variables

factor_vars <- c ("WORKCLASS", "EDUCATION", "MARITALSTATUS", "OCCUPATION", "RELATIONSHIP", "RACE", "SEX", "NATIVECOUNTRY")
continuous_vars <- c("AGE", "FNLWGT","EDUCATIONNUM", "HOURSPERWEEK", "CAPITALGAIN", "CAPITALLOSS")

iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(14))  # init for IV results

# compute IV for categoricals

for(factor_var in factor_vars){
  smb <- smbinning.factor(trainingData, y="ABOVE50K", x=factor_var)  # WOE table
  if(class(smb) != "character"){ # return error if object classes is not categorical
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}

# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb <- smbinning(trainingData, y="ABOVE50K", x=continuous_var)  # WOE table
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}

# the above info values do not consider variable interactions
iv_df <- iv_df[order(-iv_df$IV), ]  # sort

iv_df

logitMod <- glm(ABOVE50K ~ RELATIONSHIP + AGE + CAPITALGAIN + OCCUPATION + EDUCATIONNUM, data=trainingData, family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, testData))  # predicted likelihood scores for each all the obs in 
## the test dataset

## OR another menthod of creating the predicted scores using below

predicted <- predict(logitMod, testData, type="response")  # predicted scores


summary(logitMod)

library(InformationValue)
optCutOff <- optimalCutoff(testData$ABOVE50K, predicted)[1] 
#=> 0.71

# calculate the proportion of obs which were correctly classified by the log model
misClassError(testData$ABOVE50K, predicted, threshold = optCutOff)

# similar in the case of linear regression, need to inspect for multicollinearity in the model


require(car)
vif(logitMod)


plotROC(testData$ABOVE50K, predicted)

Concordance(testData$ABOVE50K, predicted)

sensitivity(testData$ABOVE50K, predicted, threshold = optCutOff)
#> 0.3089
specificity(testData$ABOVE50K, predicted, threshold = optCutOff)
#> 0.9836

confusionMatrix(testData$ABOVE50K, predicted, threshold = optCutOff)
# The columns are actuals, while rows are predicteds