library(png)
library(grid)
library(ggplot2)
library(gridExtra)

Dataset <- read.csv("SF and Non Fusion.csv")

Dataset$Year <- as.factor(Dataset$Year)
plot <- ggplot(data=Dataset)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73") 

## create jitterplot of monthly spinal fusion cost per surg
p <- plot + geom_jitter(aes(x=Year,y=Spinal_Fusion,colour=Year),alpha=0.8,size=3) + 
scale_colour_manual(values=cbPalette)+ ggtitle("Monthly Average Cost Per Espiode for Spinal Fusion Surgeries")+
labs(y="Average Cost per Surgery ($)",x="") +  geom_text(aes(x=as.numeric(Year),y=Spinal_Fusion,label = Spinal_Fusion),alpha=0.4)

## arrange and save as png file in default directory
ggsave('Spinal Fusion.png', arrangeGrob(p))

## create jitterplot of monthly spinal fusion cost per surg
p <- plot + geom_jitter(aes(x=Year,y=Non_Fusion,colour=Year),alpha=0.8,size=3) + 
scale_colour_manual(values=cbPalette)+ ggtitle("Monthly Average Cost Per Espiode for Non-Fusion Surgeries")+
labs(y="Average Cost per Surgery ($)",x="") +  geom_text(aes(x=as.numeric(Year),y=Non_Fusion,label = Non_Fusion),alpha=0.4)

## arrange and save as png file in default directory
ggsave('Non-Fusion.png', arrangeGrob(p))


### no of of items per surgery

dataset2 <- read.csv("Items counts SF and Non Fusion.csv")
dataset2$Year <- as.factor(dataset2$Year)
plot2 <- ggplot(data=dataset2)

## create jitterplot of monthly spinal fusion cost per surg
p <- plot2 + geom_jitter(aes(x=Year,y=SF,colour=Year),alpha=0.4,size=3) + 
scale_colour_manual(values=cbPalette)+ ggtitle("Monthly Average No. of items Per Spinal Fusion Surgery")+
labs(y="Average No. of items charged per service",x="") +  geom_text(aes(x=as.numeric(Year),y=SF,label = SF),alpha=0.8)

## arrange and save as png file in default directory
ggsave('Spinal Fusion_item_Cnts.png', arrangeGrob(p))

## create jitterplot of monthly spinal fusion cost per surg
p <- plot2 + geom_jitter(aes(x=Year,y=Non_Fusion,colour=Year),alpha=0.4,size=3) + 
scale_colour_manual(values=cbPalette)+ ggtitle("Monthly Average No. of items Per Non-Fusion Surgery")+
labs(y="No. of items charged per service",x="") +  geom_text(aes(x=as.numeric(Year),y=Non_Fusion,label = Non_Fusion),alpha=0.8)

## arrange and save as png file in default directory
ggsave('Non-Fusion_item_Cnts.png', arrangeGrob(p))


str(dataset2)

plot3 <- ggplot(data=dataset2)

# change excel date number into date format
dataset2$Dates = as.Date(dataset2$Dates,origin = "1899-12-30")

dataset2$Dates = as.Date(dataset2$Dates, format = " %d %Y")

## times series with loess smooth line for Spinal Fusion items cnts
p <- plot3 + geom_point(aes(x=Dates,y=SF),alpha=0.8,colour="red")+ geom_line(aes(x=Dates,y=SF))+ geom_smooth(aes(x=Dates,y=SF),method = "loess", size = 1.5)+
geom_text(aes(x=Dates,y=SF,label = SF),alpha=0.4,angle = 45) + ggtitle("Monthly Average No. of items Per Spinal Fusion Surgery")+ 
labs(y="Average No. of items charged per service",x="Year")+ 
geom_vline(xintercept=unclass(as.Date("2016-12-31")),size=2,alpha=0.5,colour="green")+
 annotate(geom="text",x=as.Date("2016-7-31"),y=5,label="Rollout of\n Pre-approval\n  Process",fontface="bold",colour="dark green",alpha=0.5)


## arrange and save as png file in default directory
ggsave('Spinal_fusion_item_Cnts_TS.png', arrangeGrob(p))

## times series with loess smooth line for non fusion items cnts
p <- plot3 + geom_point(aes(x=Dates,y=Non_Fusion),alpha=0.8,colour="red")+ geom_line(aes(x=Dates,y=Non_Fusion))+ geom_smooth(aes(x=Dates,y=Non_Fusion),method = "loess", size = 1.5)+
geom_text(aes(x=Dates,y=Non_Fusion,label = Non_Fusion),alpha=0.4,angle = 60) + ggtitle("Monthly Average No. of items Per Non-Fusion Surgery")+ 
labs(y="Average No. of items charged per service",x="Year")+ 
geom_vline(xintercept=unclass(as.Date("2016-12-31")),size=2,alpha=0.5,colour="green")+
 annotate(geom="text",x=as.Date("2016-7-31"),y=3,label="Rollout of\n Pre-approval\n  Process",fontface="bold",colour="dark green",alpha=0.5)

print(p)
## arrange and save as png file in default directory
ggsave('Spinal_fusion_item_Cnts_TS.png', arrangeGrob(p))

