## Mixed Model longitudinal analysis

## Read in the dataset
Opioids <- read.csv("~/Opioids.csv")

data <- Opioids <- read.csv("~/Opioids.csv")


## reshape the data into a long form - varied by time
Dat <- reshape(data, direction="long", 
                  varying = c("Opioids0","Opioids1","Opioids2"),
                  idvar  = c("Subject","Neuropathic_Prior","Anti_depressants_Prior",
                             "Psych_Prior","Comp_Prior","SEX_CD","Surg_type",
                             "Days_To_Surg","Age_At_Surg"),sep="")

## change Dat col names
colnames(Dat) <- c("Subject","Neuro","Anti","Psych","Comp","Sex","Surg","Days","Age","Time","Opioids")

## coerce ints into factors
Dat$Neuro  <- as.factor(Dat$Neuro)
Dat$Anti <- as.factor(Dat$Anti)
Dat$Psych <- as.factor(Dat$Psych)
Dat$Surg <- as.factor(Dat$Surg)
Dat$Time <- as.factor(Dat$Time) 
Dat$Opioids <- as.factor(Dat$Opioids) 

mod1glm = glm(Opioids~Surg*Days*Time, family=binomial, Dat)
# fit Model 2: without interaction
mod2glm = glm(Opioids~Surg+Days+Time, family=binomial, data=Dat)
# test these two model using Chi-Square test
anova(mod1glm,mod2glm, test="Chi")

summary(mod1glm)

# load the ``multcomp" library
library(multcomp)
# multiple comparisons
glht.mod2glm <- glht(mod2glm, mcp(Surg="Tukey", Time="Tukey"))
summary(glht.mod2glm)


# load the ``MASS" library
library(MASS)

## rand effect applied to intercept only
mod3glm = glmmPQL(Opioids~Surg+Days+Time, random=~1|Subject, 
                  family=binomial, Dat)


# print the summary
summary(mod2glm)

# test these two model using Chi-Square test
anova(mod2glm,mod3glm, test="Chi")

# sqrt(Residual Deviance/df) Compare against StdDev Residual of mixed effect model
## which ever one is less model is incorpated more info.  

?glmmPQL()


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

# load the ``multcomp" library
library(multcomp)
# multiple comparisons
glht.mod2glm <- glht(mod2glm, mcp(Surg="Tukey", Time="Tukey"))
summary(glht.mod2glm)


# load the ``MASS" library
library(MASS)

## rand effect applied to intercept only
modglm3 = glmmPQL(Capacity~Surg*Time*Age_At_Surg*Days_To_Surg, random=~1|Subject, 
                  family=binomial, Dat2)

modglm4 = glmmPQL(Capacity~Surg+Time+Age_At_Surg+Days_To_Surg, random=~1|Subject, 
                  family=binomial, Dat2)


# test these two model using Chi-Square test
anova(modglm3,modglm4, test="Chi")


# print the summary
summary(modglm4)


