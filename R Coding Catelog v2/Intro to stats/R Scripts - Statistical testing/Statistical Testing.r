folder.location <- "/Users/macbook/Dropbox/Courses/R_Training/"
setwd(folder.location)
# load(".RData") # uncomment if necessary
source("additional_functions.r")

##### Random number generation

# setting the seed (or not)

set.seed(1)  # use the same seed at the start of every session if you want reproducible results
# note: not setting the seed uses the current time stamp to set it....so your random numbers will be different from one session to the next

runif(10)
set.seed(2)  # use the same seed at the start of every session if you want reproducible results
runif(10)  # same as last call since we reset the seed
runif(10)  # now we keep on going and get 10 new random numbers


# random numbers from different distributions
rnorm(30, mean=35.32, sd=2)   # normal distribution mean=35.32, sd=2
rpois(300, lambda=4.5)  # poisson distribution lambda=4.5
rbeta(30, shape1=5.5, shape2=3.2)  # beta distribution, alpha=5.5, beta=3.2
#etc.


# density, cdfs, and quantiles of distributions
dnorm(2,0,1)  # value of the standard normal pdf at the point +2
pnorm(1.96,0,1)  # area under the standard normal curve up to the point +1.96
qnorm(.95,0,1)  # point in the standard normal distribution where 95% of the curve is below it 


##### Statistical tests

### Is the avg total spending for segment A customers different than for segment B?

# use t.test function

result <- t.test(customers$TotalSpending[customers$Segment=='A'],customers$TotalSpending[customers$Segment=='A'], alternative="two.sided",conf.level=.95)
class(result)  # class is "htest" which is a special class often created for holding results of hypothesis tests - it is a list-like structure
names(result)  # see the components of the result that we can view
result  # default printing of the result
result$p.value
result$statistic  # can use these as we wish in further programming...


# a little program snippet to conduct the above test for Segment A against all other segments separately

pvalues <- numeric()  # create a numeric vector of unspecified size
other.segments <- sort(setdiff(unique(customers$Segment),"A"))
for (seg in other.segments) {  # test against each other segment one at a time
  result <- t.test(customers$TotalSpending[customers$Segment=='A'],customers$TotalSpending[customers$Segment==seg],alternative="two.sided",conf.level=.95)
  pvalues <- c(pvalues,result$p.value)
}
result <- data.frame(Segments=other.segments,p.values=pvalues)
result


### Is the proportion of customers who are top customers different for segment A than segment B?

# use the prop.test function for a z-test of two proportions

countA <- sum(customers$Segment=='A' & customers$TopCustomer == 1)
nA <- sum(customers$Segment=='A')
countB <- sum(customers$Segment=='B' & customers$TopCustomer == 1)
nB <- sum(customers$Segment=='B')
countA; nA; countA/nA; countB; nB; countB/nB

result <- prop.test(c(countA,countB),c(nA,nB),alternative="two.sided",conf.level=.95)
names(result)
result



##### OPTIONAL

###Is there a relationship between a person's segment and their age grouping?

# use chisq.test to conduct a  Chi-square tests of independence on 2-dimensional tables

result <- chisq.test(customers$Segment,customers$AgeGroup)
class(result)
names(result)
result
result$p.value
result$observed
result$expected


### Computing sample size or power for a test or experiment

library(pwr)
pwr.t.test(n=NULL, d=.2, sig.level=.05, power=.9, type="two.sample") # d is the effect size in standard deviations; leave n NULL to have it compute n
pwr.t.test(n=400, d=.2, sig.level=.05, power=NULL, type="two.sample")  # leave power NULL to have it compute power given other parameters

help(pwr)  # see the other functions available for computing power

######

##### Regression modeling

# first, let's look again at formulas in R

y ~ x1+x2  # tilde is like an equals sign in a model formula (lhs is the dependent variable, rhs are the independent terms)
f <- y ~ x1+x2  # can place the formula into a named object
class(f)   # it is an object of class "formula"
f

f <- as.formula("y ~ x1+x2")  # can also create formulas from strings if want to put together a formula on the fly
f
class(f)


## lm function: basics of building a linear regression...can we predict a person's Future Value?

with(customers,plot(Age,FutureValue)) # look at an initial scatter plot of Future Value vs Age

my.model <- lm(FutureValue ~ Age, data=customers, na.action=na.exclude)
class(my.model)
my.model
summary(my.model)  # the summary function applied to a lm object returns an object itself with useful information
anova(my.model)    # same comment...the anova function applied to a lm object is an object with useful information



## extracting information

# basics
names(my.model)
my.model$coefficients  # reference the coefficients (it's a named numeric vector)

names(summary(my.model))  # the summary function provides an object with additional results from the regression (incl r.squared)
summary(my.model)$r.squared


# residuals and fitted values
resids <- residuals(my.model)  # a safe way to get residuals that accounts for missing values when na.action=na.exclude was used in the model call
head(resids)
sum(resids^2,na.rm=TRUE)   # compute sse
mean(abs(resids),na.rm=TRUE)  # mean absolute residual
fittedvals <- fitted(my.model)
head(fittedvals) # predictions for the first 6 observations in our dataset


# some diagnostics and regression assumption checking
plot(resids)   # sequence plot of residuals to look for non-randomness patterns over observations
hist(resids,breaks=20)   # histogram to look at distribution of residuals (symmetric around 0?)
plot(customers$Age,resids)    # look for variance homogeneity or non-linear patterns
plot(customers$FutureValue, fittedvals)  # plot actual versus predicted values (looking for points along the diagonal)

# plot the regression function onto the scatterplot of TotalSpending vs Age
plot(customers$Age,customers$FutureValue)
abline(my.model) # the abline low-level plotting function takes a linear model as input to automatically plot the fitted regression line


# some pre-defined diagnostic plots for lm objects
layout(matrix(c(1,2,3,4),2,2)) # fit 4 graphs on the page
plot(my.model) 
layout(1)


### adding terms to our regression: additional predictors, non-linearity, transformations, factors, interactions, subset just good data, etc.
my.model2 <- lm(FutureValue ~ log(Age) + MarketingChannel + Segment + PreviousOrders + PreviousOrders:Age + Region, 
              	data=subset(customers,Age>21), na.action=na.exclude)
my.model2
summary(my.model2)

# for everything, try: y ~ . -UnwantedFeatures


### Stepwise regression

library(MASS)  # need to load the very helpful MASS library
my.model2.step <- stepAIC(my.model2,direction="backward")  #notice the iterative output (see help(stepAIC) for more control options on how it works)
my.model2.step
summary(my.model2.step)
class(my.model2.step)   # still an lm object, so can do all same analysis/diagnostics as above



### Making predictions on new data: the predict function

newdata <- customers[seq(1,10000,100),]   # create a dataset of other customers to score against this model (use every 100th from original customers data)
nrow(newdata)
newdata$predicted <- predict(my.model2.step,newdata=newdata, type="response", interval="confidence")   # add a new column to this data frame holding the predicted values
head(newdata)
# compute the average absolute prediction error on our new data
mean(abs(newdata$predicted - newdata$FutureValue), na.rm=TRUE)  # use the fact that we had actual results from these people to see how we did




##### Logistic regression  (and generalized linear models)

my.logistic <- glm(TopCustomer ~ Age + MarketingChannel + Segment + Region, data=customers, family = binomial, na.action=na.exclude)
summary(my.logistic)

my.logistic.step <- stepAIC(my.logistic,direction="both")
summary(my.logistic.step)

newdata$predicted.TopCustomer <- predict(my.logistic.step, newdata=newdata, type="response")  # get predicted probabilities of being a top customer
head(newdata)
newdata$rn <- runif(nrow(newdata))
newdata <- sort.data.frame(newdata,~-predicted.TopCustomer+rn)  # sort it by the predicted value top to bottom and break ties by the random number we put on before
newdata$predicted.TopCustomer.decile <- ceiling(10 * (1:nrow(newdata)) / nrow(newdata))  # assign deciles
table(newdata$predicted.TopCustomer.decile)   # double check distribution
table(newdata$predicted.TopCustomer.decile,newdata$TopCustomer)   # evaluate
t <- tapply(newdata$TopCustomer, newdata$predicted.TopCustomer.decile, mean)  # TopCustomer percentage by predicted decile, for use in lift charts
t
plot(as.numeric(names(t)), t, type="b", xlab="Decile", pch=20, ylab="Predicted Top Customer Rate", main="Lift Chart")




##### Tree-like models

library(rpart)   # library for CART-like trees (classification and regression trees)

# let's add a couple other variables
customers$SegmentFactor <- factor(customers$Segment)
my.tree <- rpart(factor(TopCustomer) ~ Age + Region + PreviousOrders , data=customers, cp=.0002, na.action=na.exclude)
my.tree

plot(my.tree, uniform=TRUE)   # special function to plot rpart trees
text(my.tree, use.n=TRUE, all=TRUE, cex=.7)   # special function to put labels and other text data on the just-plotted tree

## predictions
newdata <- customers[sample(1:nrow(customers),25),]   # pretend we have a dataset of other customers to score against this model
newdata$predicted.tree <- predict(my.tree, newdata=newdata, type="prob")[,2]  # for rpart, the predictions come back with 2 columns, one for each classification

library("party")
iris_ctree <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris)

print(iris_ctree)
plot(iris_ctree)

plot(iris_ctree, type="simple")

set.seed(290875)
airq <- subset(airquality, !is.na(Ozone))
airct <- ctree(Ozone ~ ., data = airq,
               controls = ctree_control(maxsurrogate = 3))
### distribution of responses in the terminal nodes
plot(airq$Ozone ~ as.factor(where(airct)))
### get all terminal nodes from the tree
nodes(airct, unique(where(airct)))
### extract weights and compute predictions
pmean <- sapply(weights(airct), function(w) weighted.mean(airq$Ozone, w))
### the same as
drop(Predict(airct))
### or
unlist(treeresponse(airct))
### don't use the mean but the median as prediction in each terminal node
pmedian <- sapply(weights(airct), function(w)
  median(airq$Ozone[rep(1:nrow(airq), w)]))
plot(airq$Ozone, pmean, col = "red")
points(airq$Ozone, pmedian, col = "blue")
