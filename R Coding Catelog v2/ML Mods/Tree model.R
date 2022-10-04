library(tree)
library(ISLR)
library(MASS)
library(vcd)
library(ggplot2)

#Dataset <- read.table("H:/My Documents/IB Data Mart -  SF and Lam.csv", 
 # header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

Dataset <- read.csv("C:/Users/Angus/Documents/IB Data Mart -  SF and Lam.csv")
Dataset <- within(Dataset, {
  cl_flag <- as.factor(cl_flag)
})

summary(Dataset)
str(Dataset)


Agg <- aggregate(Count ~ Surgery_Type  + Spinal_Fusion_Classifications + cl_flag, data = Dataset, FUN=sum) 

mosaic(~ Surgery_Type  + Spinal_Fusion_Classifications + cl_flag, data = Dataset, shade = TRUE)


trees =tree(total_cwpi_pr~.,Dataset)
summary(trees)
plot(trees)
text(trees)


set.seed(1)
train = sample(1:nrow(Dataset), nrow(Dataset)/2)
tree.boston=tree( total_cwpi_pr ~.,Dataset,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)





