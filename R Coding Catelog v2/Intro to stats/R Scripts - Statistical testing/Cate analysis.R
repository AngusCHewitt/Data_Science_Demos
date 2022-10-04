##Categorical analysis diffs and associations of rectial cancer and response to treatment
##Accessing relationships with patients symptons

library(ggplot2)
library(vcd)
library(Rcmdr)

Rectal <- read.csv("C:/Users/Angus/Desktop/Categorical Analysis/Chemo treatment for rectial cancer.csv")
 
 Clean <- read.csv("C:/Users/Angus/Desktop/Categorical Analysis/Clean Data.csv")
  
 CatVars <- read.csv("C:/Users/Angus/Desktop/Categorical Analysis/Categorical_Vars.csv")
 

#create factors and sort levels
Rectal$Age_cate_bin <- factor(Rectal$Age_cate_bin)
 Rectal$Recode_5.Time.Intervals <- factor(Rectal$Recode_5.Time.Intervals,levels=c("<6 weeks","6-8 weeks","8-10 weeks","10-12 weeks",">12 weeks"))
  Rectal$Surg.Comp.Group <- factor(Rectal$Surg.Comp.Group,levels=c("0","1","2",">3"))

  Clean$Age_Binary <- factor(Clean$Age_Binary)
 Clean$Time_five_cate <- factor(Clean$Time_five_cate,levels=c("<6","6 to 8","8 to 10","10 to 12",">12"))
Clean$Surg_Comp_Group <- factor(Clean$Surg_Comp_Group,levels=c("0","1","2",">3"))

#boxplot of Treatment response vs Age of patients
Boxplot <- ggplot(data=Rectal,aes(x=Response,y=AGE))
 P <- Boxplot + geom_bin2d()
  P + scale_fill_gradientn(limits=c(0,25), breaks=seq(0, 25, by=5), colours=c('#2b83ba','#ffffbf','#d7191c'))+theme_classic()+ggtitle("Heatmap of Ages vs Complete Response")

# summary
  str(Clean)
  
#histogram of patient ages (negatively skewed)
p1 <- ggplot(data=Rectal,aes(x=AGE))
p1 + geom_histogram(colour="white",fill="red",bins=17)+ggtitle("Histogram of patient Age's")+labs(y="Counts",x="Age's")

## Chi square test sig. for test of association between Surg time v interval from chemo given Response
mosaic(~ Response + XRT_Weeks + XRT_Time_Surg,data=Clean,shade=TRUE)


## GLM linear model testing for associations
mod.fit <- glm(formula = Response ~ AGE + Time_interval + Time_XRT_to_Surg:Time_interval, family = binomial(logit), data = Clean)

mod.fit2 <- glm(formula = Response ~ XRT_Weeks + XRT_Time_Surg + XRT_Weeks:XRT_Time_Surg  , family = binomial(logit), data = Clean)

str(Clean)
summary(mod.fit)

#transform two time interval data fields to deal with some of the extreme obs 
Clean$Log_time_interval <- with(Clean, log(Time.interval))
Clean$Log_time_interval_surg <- with(Clean, log(Time.XRT.to.Surg))

## Scatterplot matrix of age vs two above variable grouped by response
plot <- scatterplotMatrix(~AGE+Log_time_interval+Log_time_interval_surg | Response, 
                  reg.line=lm, smooth=FALSE, spread=FALSE, span=0.5, id.n=0, diagonal= 
                    'boxplot', by.groups=TRUE, data=Clean)
##Remove NA's from Dataset
Clean_NA <- (na.omit(Clean))

## Create Data.Frame of RES vs Explantory Var. of Interest
RES <- data.frame(Clean_NA$AGE,mod.fit$residuals,Clean_NA$Response,mod.fit$fitted.values)


## Scatter plot of Age vs RES
scatterplot(mod.fit.residuals~Clean_NA.AGE | Clean_NA.Response, reg.line=lm, 
            smooth=TRUE, spread=TRUE, id.method='mahal', id.n = 2, boxplots='xy', 
            span=0.5, by.groups=TRUE, data=RES)

## Mod that includes Age^2
mod.fit2 <- glm(formula = Response ~ AGE + Time_interval + Time_XRT_to_Surg + Time_interval:Time_XRT_to_Surg , family = binomial(logit), data = Clean)
summary(mod.fit2)

#compare models goodness of fit
anova(mod.fit.Vars,mod.fit.Vars2)
BIC(mod.fit,mod.fit2,mod.fit.Vars,mod.fit.Vars2)
AIC(mod.fit,mod.fit2,mod.fit.Vars,mod.fit.Vars2)


head(Clean)

str(CatVars)

mod.fit.Vars <- glm(formula= Response/Patients ~ XRAYs_Weeks + XRT_Surg_Weeks,family=binomial(link =logit ),data=CatVars,weights=Patients)
summary(mod.fit.Vars)

#examine interaction affect between two time categories
mod.fit.Vars2 <- glm(formula= Response/Patients ~ XRAYs_Weeks + XRT_Surg_Weeks + XRAYs_Weeks:XRT_Surg_Weeks,family=binomial(link =logit ),data=CatVars,weights=Patients)
summary(mod.fit.Vars2)
Anova(mod.fit.Vars2)

mod.fit.Vars2$coefficients

#42 % more likely to have to have a positive response given xray <8 Weeks 
#compared with >= 8 Weeks
1/exp(mod.fit.Vars2$coefficients[2])

#50% time more likely to have have a positive response Surg <8 Weeks 
#compared to >= 8 Weeks
1/exp(mod.fit.Vars2$coefficients[3])

str(CatVars)



