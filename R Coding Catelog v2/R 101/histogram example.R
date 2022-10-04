data <- sample(x=1:100,size=1000,replace=TRUE)# rand sample
  
str(data) # observe data structure

length_data <- length(data) #length of dataset

Bins <- sqrt(length_data) # gold rule for no. of bins for histogram

hist(data,breaks=Bins) # plot hist of rand sample

