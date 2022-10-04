# Coded Example of FOrecast Comb.
## Goal: find the optimal fcast erorr using a combin for fcast mods.

#Load data:
data(electricity)

# plot actuals with colleciton of t.s. perdictions
str(electricity) # t.s. object
lwd0 <- 2
plot(electricity[,6], main="UK Electricity Supply, 2007 - 2017", xlab="Time", ylab="GWh", lwd=(lwd0+1), col= "black")
lines(electricity[,1], col="red", lwd= lwd0)
lines(electricity[,2], col="blue", lwd= lwd0)
lines(electricity[,3], col="green", lwd= lwd0)
lines(electricity[,4], col="pink", lwd= lwd0)
lines(electricity[,5], col="lightblue", lwd= lwd0)
legend("topright", legend=c("Actual", "ARIMA", "ETS", "Neural Net", "Damped Trend", "DOTM"), col=c("black", "red", "blue", "green", "pink", "lightblue"), lty=c(1,1,1,1,1,1), lwd=c((lwd0+1),lwd0,lwd0,lwd0,lwd0,lwd0), bty="n")

# Simulation in section 3.2: -------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

actual <- rnorm(100)
forecasts <- matrix(rnorm(1000, 1), 100, 10)
input_data <- foreccomb(actual, forecasts)

# Manual Selection of Trimming Factor:
model1 <- comb_TA(input_data, trim_factor = 0.3)
summary(model1)
names(model1)

# Assess accuracy of the combined forecast:
model1$Accuracy_Train

# Algorithm-Optimized Selection of Trimming Factor:
model2 <- comb_TA(input_data, criterion = "RMSE")
# Assess accuracy of the combined forecast:
model2$Accuracy_Train

# Empirical Application in section 4 --------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Data Preparation: ------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
train.obs <- electricity[1:84, 6]
train.pred <- electricity[1:84, 1:5]
test.obs <- electricity[85:123, 6]
test.pred <- electricity[85:123, 1:5]
input_data <- foreccomb(train.obs, train.pred, test.obs, test.pred)

#Look at cross-sectional dispersion of component forecasts:
cs_dispersion(input_data, measure = "SD", plot = TRUE)

# Forecast Combination (Estimation) ------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate Static Forecast Combinations:
SA <- comb_SA(input_data) #Simple Average
OLS_static <- comb_OLS(input_data) #Static OLS
EIG1_static <- comb_EIG1(input_data) #Static Standard Eigenvector
EIG4_static <- comb_EIG4(input_data, criterion = "MAE") #Static Trimmed Bias-Corrected Eigenvector

#Estimate Dynamic Forecast Combinations:
OLS_dyn <- rolling_combine(input_data, "comb_OLS") #Dynamic OLS
EIG1_dyn <- rolling_combine(input_data, "comb_EIG1") #Dynamic Standard Eigenvector
EIG4_dyn <- rolling_combine(input_data, "comb_EIG4", criterion = "MAE") #Dynamic Trimmed Bias-Corrected Eigenvector

# Post-fit functions ------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Summary statistics (to illustrate, only for OLS models)
summary(OLS_static)
summary(OLS_dyn)

#Values in the graph are given by:
OLS_static$Weights #Static OLS weights
colMeans(OLS_dyn$Weights) #Dynamic OLS average weights over validation set
