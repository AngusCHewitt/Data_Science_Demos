# Correlations stat
library(tidyverse)

# create 2 var. using rnorm
data = data.frame(x = rnorm(100,10,10), 
                  y = rnorm(100,20,20),
                  z= rnorm(100,30,30))



# calc correlations
cor_xy <- cor(data[1],data[2])

cor_xy <- cor(data)

as.vector(cor_xy)

plot(data$x,data$y)