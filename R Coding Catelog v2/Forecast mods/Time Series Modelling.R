source("itall.R")
gas82 = scan("gas82.dat")
oil82 = scan("oil82.dat")
 
plot(oil82,gas82,main = "Gas price index versus oil price index")
gas82=ts(gas82)
oil82=ts(oil82)
regmodel=lm(gas82~oil82) # This does the first ordinary regression.
summary(regmodel) # This gives us the regression results
acf(residuals(regmodel)) # ACF and PACF of the residuals
 
arima (residuals (regmodel), order = c(1,0,0), include.mean = FALSE) #AR(1) for residuals
 
y = ts.intersect(unemployment2, lag(unemployment2,-1)) # Create matrix with gas and lag 1 gas as elements
ynew=y[,1] - 0.4821*y[,2] # Create y variable for adjustment regression
x = ts.intersect(Cash2,lag(Cash2,-1)) # Create matrix with oil and lag 1 oil as elements
xnew=x[,1]-0.4821*x[,2] # Create x variable for adjustment regression
 
adjustreg = lm(ynew~xnew) # This is the adjustment regression
summary(adjustreg) # Results of adjustment regression
acf(residuals(adjustreg))