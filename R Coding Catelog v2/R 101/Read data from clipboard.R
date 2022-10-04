
#read data from clipboard
x <- read.table(file = "clipboard", sep = "t", header=TRUE)
str(x)

summary(x)

quantile(x$No..Days,.99)