## Programming in R

##### Vectors

# creating vectors manually with the "c" function  - "c"ombine any values together into a vector

a <- c(3,6,18.4)  # creates a vector object we've named "a" having three elements: the numbers 3, 6, and 18.4 in that order
print(a)  # see what is in our object "a"
a  # or an equivalent shortcut to print is to just type the name of an object to see what is in it
b <- c("John","Paul","George","Ringo")
b


# creating empty vectors

i <- integer(1000) # zero-filled integer vector of length 1000
i
# can also use functions such as "numeric" to create real-valued vector, "character" for strings, etc.


# the : operator to easily create integer sequences

a <- 3:9
a
b <- 9  # this is still a numeric vector, happens to be of length 1 (also called a "scalar vector")
b
a <- 3:b  # same result as above
a


# the seq and rep functions to create more complex numeric sequences or repeat a value multiple times

a <- seq(from=1, to=99, by=2)
a
# same:
a <- seq(from=1, to=99, length.out=50)
a
# same:
a <- seq(from=1, to=99, l=50)
a

b <- seq(6, -6, -.5)
b
b <- rep("Paul", 4)  # note that R is not a strongly typed language...I can change to type of data stored in an object any time (here "b" was numeric, now is character)
b
a <- c(3:9, 14, rep(16,3), seq(1,99,2))  # the 'c' function can combine almost anything into a single vector
a


# introduction to other functions that create vectors - random number generators and sampling from a set of values

a <- rnorm(25)   # 25 random numbers from the standard normal distribution: equivalent to rnorm(25,0,1)
set.seed(1)
a <- runif(10,0,100)   # 100 random numbers between 1 and 10
a
gender <- sample(c("M","F"), size=50, replace=TRUE) # simulate 50 genders, each with equal probability
gender
gender <- sample(c("M","F"), size=50, replace=TRUE, prob=c(.45,.55))  # simulate 50 genders, with 55% likelihood of F
age <- sample(25:85, size=50, replace=TRUE)  # simulate 50 ages from 25 through 85, each with equal probability
gender
age
# we now have 2 pieces of information on 50 simulated people stored in the gender and age vectors


# summarizing your vector

length(gender) #see how many elements

sum(age) # other simple summary functions: median, min, max, sd, var  (each return a numeric vector of length 1)
median(age); var(age); sd(age); min(age); max(age)  # many summary functions available

quantile(age) # show the "five-number summary" (min, 1st quartile, median, 3rd quartile, max) 
quantile(age, probs=(0:100)/100)  # return the percentiles of the vector (including min and max)
which.min(age)  #the index of the minimum value in a
which.max(age)  #the index of the maximum value in a
class(age) #see its class (numeric, integer, etc)
                                                                                           

# sorting and other simple operations on your vector

a <- sort(a)  # a is replaced with the sorted values
a
a <- sort(a,decreasing=TRUE)  # and now in decreasing order
a
unique(age)
length(unique(age))  # number of unique values of age
age # note these statements haven't modified our original age vector
                                                                                           
                                                                                           
##### Accessing individual elements of a vector with indices into the vector

# [] notation

age[2]  # the 2nd age in our vector
b <- age[20] + 1  # add one to the 20th age and place in the object b
age[20] # we haven't changed the 20th element of age; we have simply added one to it and put that result into the object b
b


# index sets

age[11:20]  # 11th through 20th ages
odd.indices = seq(1,length(gender),2)  # all odd numbers from 1 up to the number of elements in the gender vector
odd.indices
gender[odd.indices]  # the genders of all the odd-index people
age[length(age)]  #get last element w/o knowing ahead of time how many there are
age[c(1,3,5,5,3,1)]  # note that we can repeat indices as often as we like and order them as desired.
age


# adding and removing elements

age <- c(age,46)  #adds the number 46 to the end of age (note use of c function)
age <- age[-3]  # remove third element from a (negative sign on an index)
length(age)  # note we still have 50 ages since we added one, but removed one from our vector

# Note: w/o an assignment operator, the operations we perform are simply working on copies

age # current values in the age vector
age[-3]  # returns the age vector w/o the 3rd element...
age  # ... but the age vector hasn't been modified since we didn't re-assign the prior subset back into age

max.age.removed = age[-c(which.max(age), which.min(age))]
sort(age, dec=T)[1:5] # oldest 5

##### Vector-based operations

# math operations work on an element by element basis

spouse.age <- round(age + runif(50,-8,8))   # simulate a vector of spouse ages starting with age and adding/subtracting a random number between -8 and 8
(age + spouse.age) / 2  # average age of the couple
age / spouse.age  # ratio of spouse ages


#...including standard mathematical functions

sqrt(age)  # sqrt of each element of age
spouse.age^2   # square of each element of spouse.age (or spouse.age**2 also works)
log(age)
log10(age)
abs(age)
sin(age+(spouse.age*sqrt(age))) #usual order of operations


# "recycling" - be careful when applying mathematical operations to vectors that aren't the same length

a <- c(1,2,3)                           
b <- c(4,5,6,7)
d <- a+b  #notice the vector a recycled elements from the beginning to match the length of b (with warning, but still successful)
d  # how was d computed?


#### Section 1 Project A


#### missing (NA) values

a <- c(1,2,3,NA,5)   # the object NA has special meaning in R -> "no value available"
a
sum(a)
mean(a)
sum(a, na.rm=TRUE)
mean(a, na.rm = TRUE)



##### Other common data types

# factors

region <- factor(sample(c("NORTH","SOUTH","EAST","WEST"), size=50, replace=TRUE)) # generate sample of regions drawn from 4 possibilities, and "factorize" the vector 
region  # shows the data vector, and the levels of the factor
levels(region)  # returns a vector of the levels of the region factor


# logical vectors and expressions

female <- (gender=="F")  # return TRUE/FALSE's based on values in the gender vector (return value will be of same length as gender)
female
sum(female) # TRUE's treated as 1, FALSE's as 0 in mathematical operations on logical vectors (this counts number of females)
middle.aged <- (age >= 50)
middle.aged
sum(middle.aged & female)  # single & for vector-based 'and' operation
sum(middle.aged | !female)  # single | for vector-based 'or' operation; ! for not operation

is.na(gender)  #is.na function returns a logical vector where element is TRUE if the corresponding element is an NA value
sum(is.na(gender))  #count number of missing values
sum(is.na(a))


# subsetting vectors using logical expressions

male.ages <- age[gender=="M"]  # returns vector of the ages of just males (note length of male.ages is equal to the number of males in our gender vector)
mean(male.ages)
female.ages <- age[female]
mean(female.ages)
# note: the logical vector in [] should be of the same length as the vector being subsetted; resulting vector will possibly be of different length


##### Other Topics


##### conversion and type checking: "as." and "is." functions

is.numeric(age)
is.logical(female)
is.character(age)
as.numeric(c("1","3.4","993"))
as.numeric(c("4.5","ABC"))
as.character(age)


##### Quick introduction to matrices                                                                               

a <- matrix(c(2,2,3,4,4,5),nrow=2,byrow=TRUE)  # creates 2x3 matrix from the 6 given elements
b <- matrix(1:12,nrow=3)  # creates a 3x4 matrix from the 12 given elements
a
b
a*b #doesn't work -> default multiplication is element by element and these don't conform
f <- a %*% b  # matrix multiplication -> gives 2x4 matrix as result
square.matrix <- matrix(c(1,2,3,4),nrow=2)
square.matrix
determinant <- det(square.matrix)  # determinant of square matrix
inverse <- solve(square.matrix)  #inverse
solve(square.matrix,a) # solve a set of linear equations


##### Working with Objects

ls()   #list objects currently in the session
rm(d)  # remove the given objects


##### Adding addtional functions to your session with libraries and other external code files

library(datasets)  # if not already installed on machine, use menu Packages / Install Packages 
# install.packages function will download and install a package from a CRAN site


##### The OS environment and saving and re-loading objects into the session

# set the working directory
folder.location <- "/Users/macbook/Dropbox/Courses/R_Training/"
setwd(folder.location)
source("additional_functions.r")  # like an include statement in other languages...will execute the referenced r script as if it was typed into this script

# save objects
save.image() #save all objects to file ".RData" in the current working directory (this also can be done manually when shutting down your R interface)
save(list=c("a","b"),file="mytest.RData")
save(list=ls(), file ="mytest2.RData")
load(".RData") # the load function loads previously saved objects into memory



##### Creating custom output for log files or on screen

# send data and text to output text files (e.g., log files)
cat("Gender:\n", gender, "\n\nAge:\n", age, file="./export/output.txt")  # use \n for newlines
write.csv(gender, file="./export/output.csv", row.names = F, quote = F)
cat("\n\nSpouse Ages:\n", spouse.age, file="./export/output.txt", append=TRUE)  # append to existing file, otherwise overwrites


##### current date/time functions
Sys.Date()
Sys.time()
date()


##### Adjusting session options

options()  # list out all session options  - long list!
help(options) # documentation explains the options

# change output width
options(width=25)  # lines 25 characters wide
i  # don't like the look - too squished
options(width=110) # or make wider
i  # plenty of room on a widescreen monitor


options(scipen=12)  # controls use of scientific notation (12 or fewer significant digits will display in decimal format)



##### Section 1 Project B

