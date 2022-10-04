# load the library RODBC
require(RODBC)
# get the excel file from the file path

datfile <- "D:/R texts/Biostats/Clinical Trial data and R/Supplement to CTDA/datR4CTDA.xlsx"
# connect to the excel data book
getxlsbook = odbcConnectExcel2007(datfile)
# get the data from "DBP" datasheet
dat = sqlFetch(getxlsbook,"DBP")
# close the ODBC
odbcCloseAll()
# create the "diff"
dat$diff <- dat$DBP5-dat$DBP1
# show first 6 observations using function "head"
str(dat)

boxplot(diff~TRT, dat, xlab="Treatment",
        ylab="DBP Changes", las=1,col="green")


# load the library "bootstrap"
library(bootstrap)
# define a function to calculate the mean difference
# between treatment groups A to B: i.e., A-B
mean.diff = function(bn,dat)-diff(tapply(dat[bn,]$diff, dat[bn,]$TRT,mean))

# number of bootstrap
nboot = 1000
# call "bootstrap" function
boot.mean = bootstrap(1:dim(dat)[1], nboot, mean.diff,dat)

str(boot.mean)


# extract the mean differences
x = boot.mean$thetastar

# calcualte the bootstrap quantiles
x.quantile = quantile(x, c(0.025,0.5, 0.975))

# show the quantiles
print(x.quantile)

# make a histogram
  hist(boot.mean$thetastar, xlab="Mean Differences", main="")
# add the vertical lines for the quantiles
 abline(v=x.quantile,lwd=2, lty=c(4,1,4))

# create linear model for DBP diffs - & 95% confid intervals
LinearModel.9 <- lm(diff ~ Age + TRT, data=dat)
summary(LinearModel.9)
Confint(LinearModel.9, level=0.95)

