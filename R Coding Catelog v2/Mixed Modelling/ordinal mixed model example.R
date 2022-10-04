# example of ordinal mixed model 

library(ordinal)
library(tidyverse)
library(GGally)

# data struture
str(ordinal::wine)

# ggparis of ordinal wine d.s.
ggpairs(ordinal::wine)

# ordianl mod no rand slopes  
fm1 <- clm(rating ~ temp * contact, data = ordinal::wine)
fm1 ## print method
summary(fm1)
fm2 <- update(fm1, ~.-temp:contact)
anova(fm1, fm2)
drop1(fm1, test = "Chi")
add1(fm1, ~.+judge, test = "Chi")

# example of random slope
fmixed <- clmm2(rating ~ temp + contact, random = judge,Hess = TRUE, nAGQ = 10, data= ordinal::wine)
fmixed

summary(fmixed)