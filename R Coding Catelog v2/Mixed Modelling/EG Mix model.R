
library(Imer) # mixed mod package
library(tidyverse)

# load dataset
data("mtcars")
summary(mtcars)

mtcars %>%
  mutate(cylinders = as.factor(cyl)) -> dataset

## glm 
## predict the prob of occurance 

GLM.1 <- glm(cylinders ~ wt , data = dataset, family = quasibinomial)

newdata = data.frame( wt = 3)

mod <- predict(GLM.1, newdata = newdata,probability=TRUE)

head(attr(mod, "probabilities"))

?predict.glm(lme4)

## linear mixed model
## gives percentages as well

fit_lmm <- lmer(mpg ~ wt + (disp | cyl ), data = mtcars)

# residual fit
plot(fit_lmm)

fit_lmm

predict(fit_lmm, newdata = newdata)


## General linear mixed model
## predict actual 

fit_gmm <- glmer(mpg ~ disp + (1 | cyl ) , data = mtcars)

fit_gmm

predict(fit_lmm, newdata = newdata)


## General linear mixed model
## predict actual 

fit_nlmm <- nlmer(mpg ~ disp + (1 | cyl ), data = mtcars)

fit_nlmm

predict(fit_nlmm, newdata = newdata)

# package can produce prodibility of occurance for factor, categorical respose variable.
library(e1071)
model <- svm(as.factor(cyl) ~ ., data = mtcars, probability=TRUE)
pred <- predict(model, mtcars, probability=TRUE)

head(attr(pred, "probabilities"))

## Caret difference between reponse type actual and probability fun
# Predicting class
pred_class <- predict(model, test_set, type="response")
# Predicting probability of class
pred_prob <- predict(model, test_set, type="prob")