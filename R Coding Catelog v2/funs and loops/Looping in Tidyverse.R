library(tidyverse)
library(stringi)
library(stringr)

# create a tibble, 4 cols of rand no.
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# for loop which return median and store within vector
output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}


# Excercises *forloops* - 
# 1. comp mean from mtcars
str(mtcars)

# mean for each var. in mtcars
output <- vector("double",ncol(mtcars))
for(i in seq_along(mtcars)) {
output[[i]] <- mean(mtcars[[i]])  
}

## can use :: to access objects and or function without loading package
str(nycflights13::flights)


# ascertain the type of each column in the flights d.s.
output <- vector("character",ncol(nycflights13::flights))
for(i in seq_along(nycflights13::flights)) {
  output[[i]] <- typeof(nycflights13::flights[[i]])  
}


# Create d.f. with col names, class, typeof, head and tails;
## Use either a function of a loop
nyc <- nycflights13::flights ## test d.s.
x <- nycflights13::flights ## test d.s.


start_row <- as.data.frame((x[1,1:ncol(x)]))

t(start_row)

end_row <- (x[nrow(x),1:ncol(x)])


# use combination of loops and functions
data_details <- function(x) {

  colname <- names(x)
 
  # loop through each element of x to retrieve element types
  types <- vector("character",ncol(x))
  for(i in seq_along(x)) {
    types[[i]] <- typeof(x[[i]]) 
  }
  
  # loop through each element of x to retrieve element classes
  classes <- vector("character",ncol(x))
  for(i in seq_along(x)) {
    classes[[i]] <- stringi::stri_flatten(class(x[[i]])) 
  
  # capture 1st row for each col.  
  start_row <-(x[1,1:ncol(x)])
  first_obs <- as.vector(t(start_row))
  
  # capture Last row for each col.
  end_row <- (x[nrow(x),1:ncol(x)])
  last_obs <- as.vector(t(end_row))
  
  
  }
  data.frame(colname,types,classes,first_obs,last_obs) # return results as d.f.
  }

# test function  
data_details(nyc)


  
##--  Modifying an existing object --##

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# rescale rnorm var. using range 
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

?range(df$a)

rescale01(df)





