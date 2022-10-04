# running decomp function 
library(ggplot2)

# load decomp function into global environment
source('D:/Data integration/Documentation/Angus  - Projects/Decomposition product/Growth Decomposition/R Program/Growth Decomp Function.R')

# load dataset 
Dialysis <- read.csv('ANZDATA - Age and SA4 - Decomp.csv')

# Decompose 
data <- growth_decomp(Dialysis)

# visual plot

plot <- ggplot(data=data,aes(x=Variable_Names,y=Percentage_Distn))
plot + geom_bar(stat="identity")