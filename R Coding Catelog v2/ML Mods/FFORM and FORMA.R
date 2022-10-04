# fforms and fforma forecasting

# authored by Pablo Montero-Manso, Thiyanga Talagala, Rob J Hyndman and George Athanasopoulos.

# install.packages("devtools")
#devtools::install_github("robjhyndman/M4metalearning")

# install.packages("devtools")
#devtools::install_github("pmontman/tsfeatures")

# install.packages("devtools")
#devtools::install_github("pmontman/customxgboost")

# insallt M4 learning comp data
#install.packages("https://github.com/carlanetto/M4comp2018/releases/download/0.2.0/M4comp2018_0.2.0.tar.gz",
                # repos=NULL)

##-- example of t.s. ML mod

library(M4metalearning)
library(M4comp2018)
library(tsfeatures)
library(xgboost)

set.seed(31-05-2018)
# we start by creating the training and test subsets
# M4 has t.s. d.s. store within lists
indices <- sample(length(M4))
M4_train <- M4[ indices[1:15]]
M4_test <- M4[indices[16:25]]

# we create the temporal holdout version of the training and test sets
M4_train <- temp_holdout(M4_train)
M4_test <- temp_holdout(M4_test)


#this will take time
M4_train <- calc_forecasts(M4_train, forec_methods(), n.cores=3)

#once we have the forecasts, we can calculate the errors
M4_train <- calc_errors(M4_train)

# list t.s. features
M4_train <- THA_features(M4_train)

head(M4_train[[15]]$features)

# Use xboost to find the optimal forecasting weighting min. OWI errors
train_data <- create_feat_classif_problem(M4_train)
round(head(train_data$data, n=3),2)

# set hyper para for the mod 
set.seed(1345) #set the seed because xgboost is random!
meta_model <- train_selection_ensemble(train_data$data, train_data$errors)

# apply mod to the test d.s.
M4_test <- calc_forecasts(M4_test, forec_methods(), n.cores=1)
#> Warning in forecast::auto.arima(x, stepwise = FALSE, approximation =
#> FALSE): Having 3 or more differencing operations is not recommended. Please
#> consider reducing the total number of differences.
M4_test <- THA_features(M4_test, n.cores=1)

# combine weights and 
test_data <- create_feat_classif_problem(M4_test)
preds <- predict_selection_ensemble(meta_model, test_data$data)
head(preds)

# inspect forcast for each t.s.
M4_test <- ensemble_forecast(preds, M4_test)
M4_test[[1]]$y_hat

# performance measures
summary <- summary_performance(preds, dataset = M4_test)

# illustrate the importance of the top 20 features
mat <- xgboost::xgb.importance (feature_names = colnames(test_data$data),
                                model = meta_model)
xgboost::xgb.plot.importance(importance_matrix = mat[1:20], cex=1.0)


