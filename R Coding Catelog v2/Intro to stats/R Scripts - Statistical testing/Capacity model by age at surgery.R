Dataset <- read.table("H:/My Documents/Model Prob of Capacity by Age.csv", 
header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Dataset <- read.csv("Model Prob of Capacity by Age.csv")

library(tidyr) #Load tidyr and ggplot2
library(ggplot2)

#transpose the variables from muiltiple cols to one using factors to group the variables

Model <- gather(Dataset, #Data frame
                      Models, #Name of the variable to contain the original variable names
                      Probability_of_Atleast_Some_Capacity, #Name of the variable to contain the variables' values
                      Gap.12_CLIB.No_Opioids.0_Capacity.None:Gap.12_CLIB.No_Opioids.0_Capacity.Full) #The variables to be merged into long format

#plot the slope of each logistic regression model

plot <- ggplot(data=Model,aes(x=Age,y=Probability_of_Atleast_Some_Capacity,
col=Models,shape=Models))+geom_point(size=2,alpha=.7)+geom_line()+ theme_classic() + theme(legend.position="none") + annotate("text", x = 40, y = 85, label = "Gap>12_CLIB=No_Opioids=0_Capacity=Some",alpha=0.5) + annotate("text", x = 50, y = 99, label = "Gap>12_CLIB=No_Opioids=0_Capacity=Full",alpha=0.5)+ annotate("text", 
x = 30, y = 70, label = "Gap>12_CLIB=No_Opioids=0_Capacity=None",alpha=0.5)+ ggtitle("Comparison of Different Values Attached to Capacity a Month Prior to Surgery")+ labs(y="Likelihood of Atleast Some Capacity (%)",x="Age at Surgery")

plot

Dataset2 <- read.table("H:/My Documents/Model Prob of Opioids by Age.csv", 
header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)


#Dataset2 <- read.csv("Model Prob of Opioids by Age.csv")

str(Dataset2)

Model2 <- gather(Dataset2, #Data frame
                      Models, #Name of the variable to contain the original variable names
                      Probability_of_Atleast_Some_Capacity, #Name of the variable to contain the variables' values
                      Gap.12_CLIB.No_Opioids.0_Capacity.None:Gap.12_CLIB.No_Opioids.2_Capacity.None) #The variables to be merged into long format


plot2 <- ggplot(data=Model2,aes(x=Age,y=Probability_of_Atleast_Some_Capacity,
col=Models,shape=Models))+geom_point(size=2,alpha=.7)+geom_line()+ theme_classic() + theme(legend.position="none") + annotate("text", x = 35, y = 50, label = "Gap>12_CLIB=No_Opioids=1_Capacity=None",alpha=0.5) + annotate("text", x = 50, y = 60, label = "Gap>12_CLIB=No_Opioids=0_Capacity=None",alpha=0.5)+ annotate("text", 
x = 30, y = 30, label = "Gap>12_CLIB=No_Opioids=2_Capacity=None",alpha=0.5)+ ggtitle("Comparison of Different Values Attached to Opioids Prior to Surgery")+ labs(y="Probability of Atleast Some Capacity (%)",x="Age at Surgery")

plot2


Dataset3 <- read.table("H:/My Documents/Model Prob of CLIB & Gap_12 Months by Age.csv", 
header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Dataset3 <- read.csv("Model Prob of CLIB & Gap_12 Months by Age.csv")

str(Dataset3)

Model3 <- gather(Dataset3, #Data frame
                      Models, #Name of the variable to contain the original variable names
                      Probability_of_Atleast_Some_Capacity, #Name of the variable to contain the variables' values
                      Gap_12.Yes_CLIB.Yes_Opioids.0_Capacity.None:Gap_12.No_CLIB.No_Opioids.0_Capacity.None) #The variables to be merged into long format


plot3 <- ggplot(data=Model3,aes(x=Age,y=Probability_of_Atleast_Some_Capacity,
col=Models,shape=Models))+geom_point(size=2,alpha=.7)+geom_line()+ theme_classic() + theme(
legend.position="none") + annotate("text", x = 40, 
y = 35, label = "Gap>12_CLIB=Yes
Opioids=0_Capacity=None",alpha=0.5) + annotate("text", x = 30, y = 55, label = "Gap<=12_CLIB=Yes
Opioids=0_Capacity=None",alpha=0.5) + annotate("text", 
x = 40, y = 75 ,label= "Gap<=12_CLIB=No
Opioids=0_Capacity=None",alpha=0.5) + annotate("text", x = 45, y = 60, label = "Gap>12_CLIB=No
Opioids=0_Capacity=None",alpha=0.5) + ggtitle("Comparison of Different Values Attached to CLIB Prior to Surgery & Gap_12 Months")+ labs(y="Probability of Atleast Some Capacity (%)",x="Age at Surgery")

plot3





