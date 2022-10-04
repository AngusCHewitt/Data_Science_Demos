# classification modeling using Caret

# multinomial model example

data(Default, package = "ISLR")
library(caret)


str(Default)

head(Default)

# split the data into test and training mods using 75% of the data for the training set
set.seed(430)
default_idx = createDataPartition(Default$default, p = 0.75, list = FALSE)
default_trn = Default[default_idx, ]
default_tst = Default[-default_idx, ]


# Bin - glm -> Predict student default: training mod
default_glm_mod = train(
  form = default ~ .,
  data = default_trn,
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial"
)

# in sample results of the training mod
default_glm_mod$results

# summary training mod
summary(default_glm_mod)


# function calc out of sample mod accuarcy
calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

# calc out of sample acc.
calc_acc(actual = default_tst$default,
         predicted = predict(default_glm_mod, newdata = default_tst))


# Mod tuning - nearest neighbours mod (Knn)

default_knn_mod = train(
  default ~ .,
  data = default_trn,
  method = "knn",
  trControl = trainControl(method = "cv", number = 5)
)

# print mod
default_knn_mod


# Addition mod tuning: 1 to 101 K & Scaling the data
default_knn_mod = train(
  default ~ .,
  data = default_trn,
  method = "knn",
  trControl = trainControl(method = "cv", number = 5),
  preProcess = c("center", "scale"),
  tuneGrid = expand.grid(k = seq(1, 101, by = 2)))

# visualise acc vs no. k
plot(default_knn_mod)

default_knn_mod

##0-- multinomial example --##






