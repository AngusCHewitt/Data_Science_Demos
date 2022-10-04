Dataset <- read.table("H:/My Documents/Model Prob of Capacity by Age.csv", 
header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

library(tidyverse) #Load all tidyverse packages

Model <- gather(Dataset, #Data frame
                      Models, #Name of the variable to contain the original variable names
                      Probability_of_Atleast_Some_Capacity, #Name of the variable to contain the variables' values
                      Gap.12_CLIB.Yes_Opioids.2_Capacity.None:Gap.12_CLIB.Yes_Opioids.2_Capacity.Full) #The variables to be merged into long format


plot <- ggplot(data=Model,aes(x=Age,y=Probability_of_Atleast_Some_Capacity,
col=Models,shape=Models))+geom_point(size=2,alpha=.7)+geom_line()+theme(
legend.position="none") + annotate("text", x = 40, y = 50, label = "Gap>12_CLIB=Yes_Opioids=2_Capacity=Some") + annotate("text", x = 50, y = 80, label = "Gap>12_CLIB=Yes_Opioids=2_Capacity=Full")+ annotate("text", 
x = 30, y = 30, label = "Gap>12_CLIB=Yes_Opioids=2_Capacity=None")+ ggtitle("Comparison of Different Values Attached to Capacity a Month Prior to Surgery")+ labs(y="Likelihood of Atleast Some Capacity (%)",x="Age at Surgery")

plot

Dataset2 <- read.table("H:/My Documents/Model Prob of Opioids by Age.csv", 
header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

Model2 <- gather(Dataset2, #Data frame
                      Models, #Name of the variable to contain the original variable names
                      Probability_of_Atleast_Some_Capacity, #Name of the variable to contain the variables' values
                      Gap.12_CLIB.Yes_Opioids.0_Capacity.None:Gap.12_CLIB.Yes_Opioids.2_Capacity.None) #The variables to be merged into long format


plot2 <- ggplot(data=Model2,aes(x=Age,y=Probability_of_Atleast_Some_Capacity,
col=Models,shape=Models))+geom_point(size=2,alpha=.7)+geom_line()+theme(
legend.position="none") + annotate("text", x = 30, y = 35, label = "Gap>12_CLIB=Yes_Opioids=1_Capacity=None",alpha=0.5) + annotate("text", x = 30, y = 50, label = "Gap>12_CLIB=Yes_Opioids=0_Capacity=None",alpha=0.5)+ annotate("text", 
x = 40, y = 20, label = "Gap>12_CLIB=Yes_Opioids=2_Capacity=None",alpha=0.5)+ ggtitle("Comparison of Different Values Attached to Opioids Prior to Surgery")+ labs(y="Probability of Atleast Some Capacity (%)",x="Age at Surgery")

plot2



Dataset3 <- read.table("H:/My Documents/Model Prob of CLIB & Gap_12 Months by Age.csv", 
header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

Model3 <- gather(Dataset3, #Data frame
                      Models, #Name of the variable to contain the original variable names
                      Probability_of_Atleast_Some_Capacity, #Name of the variable to contain the variables' values
                      Gap.12_CLIB.Yes_Opioids.0_Capacity.None:Gap.12_CLIB.Yes_Opioids.2_Capacity.None) #The variables to be merged into long format


plot3 <- ggplot(data=Model3,aes(x=Age,y=Probability_of_Atleast_Some_Capacity,
col=Models,shape=Models))+geom_point(size=2,alpha=.7)+geom_line()+theme(
legend.position="none") + annotate("text", x = 30, y = 35, label = "Gap>12_CLIB=Yes_Opioids=1_Capacity=None",alpha=0.5) + annotate("text", x = 30, y = 50, label = "Gap>12_CLIB=Yes_Opioids=0_Capacity=None",alpha=0.5)+ annotate("text", 
x = 40, y = 20, label = "Gap>12_CLIB=Yes_Opioids=2_Capacity=None",alpha=0.5)+ ggtitle("Comparison of Different Values Attached to Opioids Prior to Surgery")+ labs(y="Probability of Atleast Some Capacity (%)",x="Age at Surgery")

plot3

