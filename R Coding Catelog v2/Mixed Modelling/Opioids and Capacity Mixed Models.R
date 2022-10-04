library(multcomp)
library(gee)
library(MASS)

source(file= "H:/My Documents/Categorical analysis/Chapter5/Examine.logistic.reg.R")
source(file= "H:/My Documents/Categorical analysis/Chapter5/glmDiagnostics.R")

## examine.logistic.reg() pg322 categorical analysis 


## Mixed Model longitudinal analysis

## Read in the dataset
Dataset <- read.table("H:/My Documents/SF Opioids.csv", header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)

str(Dataset) # view data structure

## reshape the data into a long form - varied by time
Dat <- reshape(Dataset, direction="long", 
                  varying = c("Opioids0","Opioids1","Opioids2"),
                  idvar  = c("Subject","treatment","Anti_depressants_Prior",
                             "Psych_Prior","Comp_Prior","SEX_CD","Surg_type",
                             "Days_To_Surg","Age_At_Surg"),sep="")

## order factor - time
Dataset$variable <- with(Dataset, factor(variable, levels=c('1','2','3'), 
+   ordered=TRUE))

## change Dat col names
colnames(Dat) <- c("Subject","Neuro","Anti","Psych","Comp","Sex","Surg","Days","Age","Time","Opioids")

## coerce ints into factors
Dat$Neuro  <- as.factor(Dat$Neuro)
Dat$Anti <- as.factor(Dat$Anti)
Dat$Psych <- as.factor(Dat$Psych)
Dat$Surg <- as.factor(Dat$Surg)
Dat$Opioids <- as.factor(Dat$Opioids)

mod1glm = glm(Opioids~Surg*Days*Time, family=binomial, Dat)
# fit Model 2: without interaction
mod2glm = glm(Opioids~Surg+Days+Time, family=binomial, data=Dat)
# test these two model using Chi-Square test
anova(mod1glm,mod2glm, test="Chi")

summary(mod1glm)

# multiple comparisons
glht.mod2glm <- glht(mod2glm, mcp(Surg="Tukey", Time="Tukey"))
summary(glht.mod2glm)


## rand effect applied to intercept only
mod3glm = glmmPQL(Opioids~Surg+Days+Time, random=~1|Subject, 
                  family=binomial, Dat)


# check the models summaary stats
summary(mod2glm)

# test these two model using Chi-Square test
anova(mod2glm,mod3glm, test="Chi")

# sqrt(Residual Deviance/df) Compare against StdDev Residual of mixed effect model
## which ever one is less model is incorpated more info.  


# worker Capacity
Capacity <- read.csv("~/Worker Capacity.csv")

View(Capacity)


## reshape the data into a long form - varied by time
Dat2 <- reshape(Capacity, direction="long", 
               varying = c("No_Some_Comp0","No_Some_Comp1","No_Some_Comp2"),
               idvar  =  c("Subjects","Opioids_0_3_prior","Neuropathic_0_3_prior",
                          "Anti_depressants_0_3_prior","Psych_0_3_prior",
                          "SEX_CD","ELECSURG_SRVITEM","IRD_to_Surgery",
                          "Age_At_Surg"),sep="")

## change Dat col names
colnames(Dat2) <- c("Subject","Opioids","Neuro","Anti","Psych","Sex","Surg","Days_To_Surg","Age_At_Surg","Time","Capacity")

## coerce ints into factors
Dat2$Neuro  <- as.factor(Dat2$Neuro)
Dat2$Anti <- as.factor(Dat2$Anti)
Dat2$Psych <- as.factor(Dat2$Psych)
Dat2$Surg <- as.factor(Dat2$Surg)
Dat2$Time <- as.factor(Dat2$Time) 
Dat2$Opioids <- as.factor(Dat2$Opioids) 

str(Dat2)


modglm1 = glm(Capacity~Surg*Time*Age_At_Surg*Days_To_Surg, family=binomial, Dat2)
# fit Model 2: without interaction
modglm2 = glm(Capacity~Surg+Time+Age_At_Surg+Days_To_Surg, family=binomial, data=Dat2)
# test these two model using Chi-Square test
anova(modglm1,modglm2, test="Chi")

summary(modglm2)

# multiple comparisons
glht.mod2glm <- glht(mod2glm, mcp(Surg="Tukey", Time="Tukey"))
summary(glht.mod2glm)


## rand effect applied to intercept only
modglm3 = glmmPQL(Capacity~Surg*Time*Age_At_Surg*Days_To_Surg, random=~1|Subject, 
                  family=binomial, Dat2)

## rand effect incorporated into the intercept & slope
modglm4 = glmmPQL(Capacity~Surg+Time+Age_At_Surg+Days_To_Surg, random=~Time|Subject, 
                  family=binomial, Dat2)


# test these two model using Chi-Square test
anova(modglm3,modglm4, test="Chi")


# print the summary
summary(modglm4)


