folder.location <- "/Users/macbook/Dropbox/Courses/R_Training/"
setwd(folder.location)
# load(".RData") # uncomment if necessary
source("additional_functions.r")


##### Base graphics

segments<-unique(customers$Segment)
# let's add a simulated FutureValue variable to our dataset
customers$FutureValue <- round((100-customers$Age)*100 + 50*match(customers$Segment,segments) + rnorm(nrow(customers),0,350))

## quick 2-d scatter plot
# pch: plot symbol (character)
plot(customers$Age,customers$FutureValue, type="p", pch=21, col="black", bg="red", xlab="Age", ylab="Estimated Future Value",main="Plot 1")


## same thing, but let's color code and provide different symbols by segment
my.colors <- c(A="red",B="blue",C="green",N="brown")  # create named vector
my.symbols <- c(A=21, B=22, C=23, N=24)  # create named vector


# Now let's make the plot
plot(customers$Age, customers$FutureValue, type="p", bg=my.colors[customers$Segment], pch=my.symbols[customers$Segment], main="Future Value versus Age") # note the points fill the plot in the order encountered
legend(0,7000, legend=segments, pt.bg=my.colors, pch=my.symbols, title="Segment", yjust=1, bty="n") # bty: box type, x & y: coordinates of the upper left corner of the legend box


# Annotate the scatter plot's "outlying" points
outliers <- subset(customers, Age < 20)  #define "outlier" however you want
outliers   # 2 meet the criteria
text(outliers$Age, outliers$FutureValue, outliers$CustomerID, cex=.75, col="dark blue", pos=4)  # cex controls text size, pos controls fine tuned positioning


## Bar charts using barplot function, and label bars with heights
means <- tapply(customers$FutureValue, customers$Segment, mean, na.rm=TRUE)[1:3] # don't use the N segment
bar.color <- rgb(red=17, blue=107, green=64, maxColorValue=255) # can specify custom colors using the rgb function 

bar.xpositions <- barplot(means, names.arg=names(means), col=bar.color, border="dark blue", ylim=c(0,1.25*max(means)), main="Future Value Analysis", xlab="Segment", ylab="Mean Estimated Future Value")
bar.xpositions # the plot was created, and the x-coordinates of the centers of the bars are returned...let's use them to annotate the plot

text(bar.xpositions, means, format(round(means,2),nsmall=2), pos=3)  # place labels above the top of the bar, adjusted slightly upward
abline(h=1500, lty='dashed')  #maybe want to show a baseline comparison value


## histograms and density plots for numeric fields
hist(customers$Age, breaks=10, col=bar.color, xlab="Age", ylab="Counts", main="Histogram for Age")  # default setting for breaks parameter automatically determines histogram cutpoints


plot(density(customers$Age), xlab="Age", ylab="Counts", main="Estimated Density for Age")  # plot function looks for the x and y components of the age.density object
polygon(density(customers$Age), col='red') #low level plotting function polygon to shade in a polygon region


## all possible pairs of numeric fields, color coded by segment
pairs(subset(customers,TRUE,c("FutureValue","Age","PreviousOrders")), pch=21, bg=my.colors[match(customers$Segment,segments)])  # strong (negative) association between Age and FutureValue


## time series data and plot
tsdata <- read.csv("data/airpass.csv")$AirPass
tsdata <- ts(tsdata, start=c(1960,1), frequency=12) # make it a time series object
tsdata
plot(tsdata, main="Number of Airline Passengers", ylab="# Passengers", lwd=1.5, col='dark blue')  #special plot functionality if we pass in time series data



## cool correlation plot

library(corrgram)
corr.variables <- sapply(customers,is.numeric)
corrgram(customers[,corr.variables], order=TRUE, row1attop=FALSE, main="Key Customer Dataset Correlations",upper.panel=panel.ellipse, lower.panel=panel.shade, diag.panel=panel.density)



## manipulating/generating colors in R: see rgb, rainbow, colors, terrain.colors, heat.colors, hsv, and RColorBrewer package
rainbow(10)
pie(rep(1,20), col=rainbow(20))
pie(rep(1,20), col=terrain.colors(20))
pie(rep(1,20), col=heat.colors(20))



### Simple mapping using the maps package
library(maps)
library(mapproj)

# let's show average Previous Orders by state
mean.prev.ord <- tapply(customers$PreviousOrders, customers$State, mean, na.rm=TRUE)
category <- cut(mean.prev.ord, breaks=quantile(mean.prev.ord,probs=seq(0,1,by=.05)), labels=FALSE, include.lowest=TRUE)
names(category) <- names(mean.prev.ord)

mp <- map("state", plot=FALSE,namesonly=TRUE)  # get the state names used by the map
stnames <- sapply(strsplit(mp,":"),function(l)l[1]) # clean them up (deal with islands)
abbs <- state.abb[match(stnames,tolower(state.name))]  # and match official state abbreviations to them

map("state", col=heat.colors(20)[(category[abbs])], fill=TRUE, lty = 1, lwd =1, projection = "polyconic", resolution = 0) # draw the map
mtext("Avg Previous Orders by State Heat Map") # use mtext to put a title in the top margin

## also see http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html for more examples of maps and pointers to other packages




##### Layout multiple plots on the same page - using the layout function

mylayout = matrix(c(1,2,3,3),nrow=2,ncol=2,byrow=TRUE)
mylayout  # 2x2 grid of plots: plot 1 on upper left, plot 2 on upper right, plot 3 covering entire bottom
layout(mylayout)  

# next 3 high level plotting commands sent will fill the page (same graphs as we did above)
plot(customers$Age,customers$FutureValue, type="p", pch=21, col="black", bg="red", xlab="Age", ylab="Estimated Future Value",main="My Plot")
hist(customers$Age, breaks=10, col=bar.color, xlab="Age", ylab="Counts", main="Histogram for Age")  # default setting for breaks parameter automatically determines histogram cutpoints
with(customers,{
  plot(Age, FutureValue, type="p", bg=my.colors[Segment], pch=my.symbols[Segment], main="Future Value versus Age")
  legend(max(Age), max(FutureValue), legend=segments, pt.bg=my.colors, pch=my.symbols, title="Segment", xjust=1, yjust=1, bty="n")
}
)  # note the points fill the plot in the order encountered
outliers <- subset(customers, Age < 20)  #define "outlier" however you want
text(outliers$Age, outliers$FutureValue, outliers$CustomerID, cex=.75, col="dark blue", pos=4)  # cex controls text size, pos controls fine tuned positioning

layout(1)  # reset to one per page

# also see par options oma and mai for setting up margins of the page relative to the plot region, and mtext for placing text in the margins


### Project 5A



##### "Trellis" graphics is a different and very powerful graphics engine in R

library(lattice)
help(lattice)


## xy scatter plots
xyplot(FutureValue~Age,data=customers,xlab="Age",ylab="Estimated Future Value",main="My Trellis Graph by Segment")
xyplot(FutureValue~Age|Segment,data=customers,xlab="Age",ylab="Estimated Future Value",main="My Trellis Graph by Segment")

# adding a panel function to add more "stuff" to each panel automatically
xyplot(FutureValue~Age|Segment,data=customers,xlab="Age",ylab="Estimated Future Value",main="My Trellis Graph by Age Bracket",
       panel = function(x, y) {
           panel.grid(h=-1, v= 2)  # add a grid
           panel.xyplot(x, y)  # draw the actual scatter plot
           panel.rug(x,y) # add marginal distribution information for x and y
           outliers <- data.frame(x=x[x<20],y=y[x<20])
           panel.text(outliers$x, outliers$y, "outlier", cex=.6, col="red", pos=4)
           panel.lines(x=c(mean(x),mean(x)), y=c(min(y),max(y)), lty='dashed', col="purple")
       }
)


## histograms
histogram(~Age|Segment,data=customers,col="dark green",main="My Histogram")  # easy to break it out by segment


## scatter plot matrix
splom(~customers[,c("Age","PreviousOrders","FutureValue")] | Segment, data=customers, axis.text.cex=.4, varname.cex=.5)


## contour plot example (from R documentation)

attach(environmental)
ozo.m <- loess((ozone^(1/3)) ~ wind * temperature * radiation, parametric = c("radiation", "wind"), span = 1, degree = 2)
w.marginal <- seq(min(wind), max(wind), length.out = 50)
t.marginal <- seq(min(temperature), max(temperature), length.out = 50)
r.marginal <- seq(min(radiation), max(radiation), length.out = 4)
wtr.marginal <- list(wind = w.marginal, temperature = t.marginal, radiation = r.marginal)
grid <- expand.grid(wtr.marginal)
grid[, "fit"] <- c(predict(ozo.m, grid))
contourplot(fit ~ wind * temperature | radiation, data = grid,
            cuts = 10, region = TRUE,
            xlab = "Wind Speed (mph)",
            ylab = "Temperature (F)",
            main = "Cube Root Ozone (cube root ppb)")
detach()



## other very cool examples can be found on the web:
# http://gallery.r-enthusiasts.com/
# http://www.sr.bham.ac.uk/~ajrs/R/r-gallery.html



#### Exporting and saving graphics

## from the menu of the graphics window, or...

## programatically...

pdf("./export/chart.pdf",width=11,height=8.5)  # will send plots to this pdf file instead of to the graphics window, adding a new page for each plot

# below is just cut/pasted code from above, but output sent to pdf file
plot(customers$Age,customers$FutureValue, type="p", pch=21, col="black", bg="red", xlab="Age", ylab="Estimated Future Value",main="My Plot")
hist(customers$Age, breaks=10, col=bar.color, xlab="Age", ylab="Counts", main="Histogram for Age")  # default setting for breaks parameter automatically determines histogram cutpoints
with(customers,{
  plot(Age, FutureValue, type="p", bg=my.colors[Segment], pch=my.symbols[Segment], main="Future Value versus Age")
  legend(max(Age), max(FutureValue), legend=segments, pt.bg=my.colors, pch=my.symbols, title="Segment", xjust=1, yjust=1, bty="n")
}
)  # note the points fill the plot in the order encountered
outliers <- subset(customers, Age < 20)  #define "outlier" however you want
text(outliers$Age, outliers$FutureValue, outliers$CustomerID, cex=.75, col="dark blue", pos=4)  # cex controls text size, pos controls fine tuned positioning

dev.off()  #turn off the redirection of plot data
# now look at the pdf we've created


# see pdf documentation for the many options for controlling pdf creation
# other functions for exporting graphics: jpeg, bmp, tiff, png, postscript


### Project 5B



