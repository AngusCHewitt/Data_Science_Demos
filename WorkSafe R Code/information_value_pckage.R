install.packages("InformationValue")

library("InformationValue")

data('ActualsAndScores')

str(ActualsAndScores)

ActualsAndScores$Actuals <- as.factor(ActualsAndScores$Actuals)

plot(ActualsAndScores)

# sensitivity against specificity
plotROC(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores)
