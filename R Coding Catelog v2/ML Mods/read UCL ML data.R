## readin UCL ML data

sonar.dt <- read.table( "https://archive.ics.uci.edu/ml/machine-learning-databases/undocumented/connectionist-bench/sonar/sonar.all-data",
                          sep=",")  # header=FALSE is default for read.table

## applied modelling dataset to compliment textbook
library("AppliedPredictiveModeling")
install.packages("AppliedPredictiveModeling")
data("abalone")
summary(abalone)

