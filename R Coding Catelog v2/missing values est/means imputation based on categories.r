folder.location <- "C:/RCoursefiles"
setwd(folder.location)
# load(".RData") # uncomment if necessary
source("additional functions.r")


## Function Writing Example (3)  - Advanced for your own review ## 
# function to impute missing values for one variable based on means by another variable
# if newName is provided, a new field is created; otherwise it overwrites the existing imputeVar variable

my.imputation <- function(x, imputeVar, categoryVar, newName=NULL) {
  # compute the means of the impute variable by the category variable
  imputedMeans <- tapply(x[[imputeVar]],x[[categoryVar]],mean,na.rm=TRUE)

  if (is.null(newName)) newName=imputeVar
  
  #make the imputation using ifelse
  x[[newName]] = ifelse(is.na(x[[imputeVar]]), imputedMeans[ x[[categoryVar]] ], x[[imputeVar]])
  # key is the second parameter to ifelse in above call.  
    
  #send back the result
  x
}

# create some data to test out
mydata <- data.frame(Age=rnorm(50,55,4),Gender=sample(c("M","F"),size=50,replace=TRUE))
mydata
mydata$Age[sample(1:50,size=10)] <- NA # create 10 missing values in the Age field
mydata
mydata <- my.imputation(mydata, "Age", "Gender", "AgeNew")
mydata


# let's "functionize" our small simulation application from Section 4

mysim <- function(sample_size=30, num_reps=1000, distribution="uniform") {  # provide default values for all arguments
  means <- numeric(num_reps)
  for (i in 1:num_reps) {
    if (distribution=="uniform") {
      x <- runif(sample_size)
    } else if (distribution=="normal") {
      x <- rnorm(sample_size)
    } else {
      x <- NA
    }
    means[i] <- mean(x)
  }

  result <- list()  # create a return list object that will hold a number of nice things for the user to look at
  result$mean <- mean(means)
  result$median <- median(means)
  result$sd <- sd(means)
  result$pct5 <- quantile(means,.05)
  result$pct95 <- quantile(means,.95)
  result$sample_size <- sample_size   # and also place the input into the object fyi for the user
  result$num_reps <- num_reps
  result$distribution <- distribution
  
  class(result) <- "mysimresult"   # can even give it a a class of my own making if I want (but really only useful when we really get into object-oriented R programming)

  result  #return this list object as the output of the function
}

# use it
mysim()   # use all defaults
mysim(100,1000)
mysim(100,1000,"normal")
x <- mysim(1000,1000,"normal")
x
class(x)



#### ADVANCED/OPTIONAL 
## Example - last improvements to the simulation function (requires introduction of try function and eval/parse functions) 

## error catching with try function
x <- try(ls())
x

x <- try(lsss())
x

x <- try(lsss(),silent=TRUE)  # can execute a bad statement w/o an R error message to the user
class(x)  # notice that x has class "try-error"...we can use this fact to catch the error and display our own message or stop execution under our terms
if (class(x)=="try-error") cat("Please don't write bad R code\n\n")


### eval/parse functions -> programatically creating code in character strings
class(x)  # as we've seen, reports the class of x
str <- "class(x)"  # now, let's put that code into a character string object
str
# now let's run the class function by having R evaluate this character string
eval(parse(text=str))    # exactly the same as having types class(x) as we did above, but I created the code on the fly and put into a character string

# nice and interesting, but how can we use it to improve our R programming capabilities.....let's put some of this together...


# one last attempt to further generalize and optimize this simulation function
# 1) have no restriction on the distribution that can be provided by the user
# 2) allow additional parameters to "blindly" be sent into the random number generating function
# 3) error-catching functionality
# 4) reduce code that fills our list object return value

mysim <- function(sample_size=30, num_reps=1000, distribution="unif", ...) {
  means <- numeric(num_reps)
  
  call.string <- paste("r",distribution,"(sample_size,...)",sep="")
  for (i in 1:num_reps) {
    x <- try(eval(parse(text=call.string)), silent=TRUE)
    if (class(x)=="try-error") stop(distribution, " is not a supported distribution, or enough parameters not provided")
    means[i] <- mean(x)
  }

  result <- list(mean = mean(means), median = median(means), sd = sd(means), pct5 = quantile(means,.05), pct95 = quantile(means,.95), sample_size = sample_size,
                num_reps = num_reps, distribution = distribution)
  
  class(result) <- "mysimresult"
  result
}

mysim(distribution="norm", mean=30, sd=3)
mysim(distribution="chisq", df=14)
mysim(distribution="baddist")

# above is a more succinct, more functional version of our simulation function



##### S3 Classes, Generic functions, and methods

methods(summary)
methods(mean)
summary.data.frame
summary.lm
summary.default

summary(customers)
summary(c(1:100))


myvar <- list(m = matrix(c(1,2,3,4),nrow=2), vec = 1:25)
class(myvar)<-"myclass"
myvar
mean.myclass<-function(x) {(mean(x$m)+mean(x$vec)) / 2}
methods(mean)
mean(myvar)

#brand new method handler
addup <- function(x,...) UseMethod("addup")
addup.numeric <- function(x) {sum(x)}
addup(c(1,2,3,4))

addup.myclass <- function(x) {sum(x$m)+sum(x$vec)}
addup(myvar)
#note: can't naturally add up a matrix and vector, so the class provided additional functionality
matrix(c(1,2,3,4),nrow=2) + 1:25  # produces error



##### S4 Classes - more formal 
# "slots" in R <=> "properties" in other OOP

# let's define a class to hold a bunch of lm regression models

setClass("myModels", 
	representation(models="list",data="data.frame"), 
	prototype(models=list(),data=data.frame())
)



mymods <- new("myModels",models=list(my.model,my.model2.step),data=customers)

setMethod("summary","myModels",
  function(object) {
    mysum<-0
    len<-length(object@models)
    for (i in 1:len) mysum <- mysum + summary(object@models[[i]])$r.squared
    mysum/len
  }
)
summary(mymods)

setMethod("summary","myModels",
  function(object,extra) {
    mysum<-0
    len<-length(object@models)
    for (i in 1:len) mysum <- mysum + summary(object@models[[i]])$r.squared
    mysum/len+extra
  }
)
summary(mymods,15)


setMethod("length","myModels",
  function(x) {
    length(x@models)
  }
)

length(mymods)


# let's add a completely new method type into R that we might use for our myModels class
addModel <- function(object,m) {}
setGeneric("addModel")

setMethod("addModel",signature(m="lm"),
  function(object,m) {
    currlength <- length(object@models)
    object@models[[currlength+1]] <- m
    invisible(object)
  }
)

mymods <- addModel(mymods,my.model2.step)
length(mymods)


bestModel <- function(object) {}
setGeneric("bestModel")

setMethod("bestModel","myModels",
  function(object) {
    len <- length(object@models)
    best <- NULL
    bestrsq <- (-1)
    for (i in 1:len) {
      thisrsq <- summary(object@models[[len]])$r.squared
      if (thisrsq > bestrsq) {
        best <- object@models[[len]]
        bestrsq <-  thisrsq
      }
    }
    best
  }
)

mybest <- bestModel(mymods)
mybest


save.image()



##### Using list.files to read a directory and process files

list.files("./example7")  # get all files in the example7 subfolder of our working directory
myfiles <- list.files("./example7")   # as always, the result is just a regular character vector of file name, so let's use it programmatically
class(myfiles)

myfiles <- list.files("./example7", pattern="*.csv")   #use the pattern parameter to list.files to just select csv files
myfiles   # now contains a list of only the 3 we want

# let's append all of them together into a single data frame

mydf <- data.frame()
for (file.name in myfiles) {
  d <- read.csv( paste("./example7/",file.name,sep=""), stringsAsFactors=FALSE )
  mydf <- rbind(mydf,d)
}

nrow(mydf)



##### ADVANCED/OPTIONAL for your own information and practice


## Exporting Reports

library(R2PPT)
library(rcom)
library(RDCOMClient)
installstatconnDCOM()

#First let's create and save some graphics using the customers data frame
#We've seen all these before.

png('./html/base_plot.png')
with(customers, plot(Age,FutureValue,type="p",col=my.colors[match(Segment,segments)],pch=my.symbols[match(Segment,segments)])) 
outliers <- subset(customers,Age>0 & Age<20)  #define "outlier" however you want
outliers   # 2 meet the criteria
text(outliers$Age,outliers$FutureValue,outliers$CustomerID,cex=.5,pos=4)
dev.off()

png('./html/multiple_plot.png')
mylayout = matrix(c(1,2,3,3),nrow=2,ncol=2,byrow=TRUE)
mylayout  # 2x2 grid of plots: plot 1 on upper left, plot 2 on upper right, plot 3 covering entire bottom
layout(mylayout)
plot(customers$Age,customers$FutureValue,type="p",pch=20,col="blue",xlab="Age",ylab="Estimated Future Value",main="My Plot")
pie(counts,names(counts),main="My Pie Chart")
with(customers, plot(Age,FutureValue,type="p",col=my.colors[match(Segment,segments)],pch=my.symbols[match(Segment,segments)])) 
legend(80,10000,legend=segments,col=my.colors,pch=20,title="Segment",xjust=.5)
layout(1)  # reset to one per page
dev.off()



#Now use R2PPT to create a slide show
#Initialize the the presentation
pres <- PPT.Init(method='RDCOMClient')
#add a title slide
pres <- PPT.AddBlankSlide(pres)
pres <- PPT.AddTitleSlide(pres,title="Lityx Training",subtitle="Basic R For Business")
pres <- PPT.AddTextSlide(pres,title="Why R",text="Open Source \rThounds of packages \rFree",text.font="Arial")
#Graphics requires a blank slide
pres <- PPT.AddBlankSlide(pres)
pres <- PPT.AddGraphicstoSlide(pres,file="./html/base_plot.png")
pres <- PPT.AddBlankSlide(pres)
pres <- PPT.AddGraphicstoSlide(pres,file="./html/multiple_plot.png")
pres <- PPT.SaveAs(pres,file="./html/R Training Demo.ppt")
pres <- PPT.Present(pres)



### Creating html output using brew

#Brew takes a template file, executes R commands within it and
#outputs a either an HTML file, TEX file or pdf depending on template file
#subsequent commands. Open brew_demo.RHTML in notepad
#Brew uses only a few tags:
#	<% commands %> all R commands are executed
#	<%= commands %> execute R commands and print output to target file
#	<%= commands -%> same as above but no new line
library(brew)
brew('./html/brew_demo.RHTML', './html/brew_demo.html')



### R2HTML - create html output
library(R2HTML)
unlink("./html/customers.html") # delete our planned output file if it already exists
HTML(as.title("Our Customers Dataset"),file="./html/customers.html")
HTML(customers[1:10,1:10],"./html/customers.html")
HTML("<img src='./html/base_plot.png'>","./html/customers.html")


# also see hwriter package
