## Mixed Model longitudinal analysis

## Read in the dataset
SF_Comp <- read.csv("~/SF Comp Level Pre and Post Surg.csv")


## reshape the data into a long form - varied by time
Dat <- reshape(SF_Comp, direction="long", 
               varying = c("No_Some_Comp0","No_Some_Comp1","No_Some_Comp2"),
               idvar  = c("Subject","Opioids_0_3_prior","Neuropathic_0_3_prior",
                          "Anti_depressants_0_3_prior","Psych_0_3_prior","SEX_CD","MBS_itemcode",
                          "IRD_to_Surgery","Age_At_Surg","Years_of_Surg","Capacity__month_prior_to_surgery")
               ,sep="")

## change Dat col names
colnames(Dat) <- c("Subject","Opioid","Neuro","Anti","Psych","Sex","Surg_Type","Days_To_Surg","Age_At_Surg","Year_of_Surg", "capacity_Month_Prior","Time","Comp")


str(Dat)

## coerce ints into factors
Dat$Neuro  <- as.factor(Dat$Neuro)
Dat$Anti <- as.factor(Dat$Anti)
Dat$Psych <- as.factor(Dat$Psych)
Dat$Surg_Type <- as.factor(Dat$Surg_Type)
Dat$Time <- as.factor(Dat$Time) 
Dat$Opioid <- as.factor(Dat$Opioid) 

mod1glm = glm(Comp~ Opioid * Psych * capacity_Month_Prior * Time, family=binomial, Dat)
# fit Model 2: without interaction
mod2glm = glm(Comp~ Opioid + Psych + capacity_Month_Prior + Time, family=binomial, data=Dat)
# test these two model using Chi-Square test
anova(mod1glm,mod2glm, test="Chi")

summary(mod1glm)

# load the ``multcomp" library
library(multcomp)
# multiple comparisons
glht.mod2glm <- glht(mod1glm)
summary(glht.mod2glm)


?glht()

# load the ``MASS" library
library(MASS)

## rand effect applied to intercept only
mod3glm = glmmPQL(Comp~ Opioid * Psych * capacity_Month_Prior * Time, random=~1|Subject, 
                  family=binomial, Dat)

## rand effect applied to intercept only
mod4glm = glmmPQL(Comp~ Opioid + Psych + capacity_Month_Prior + Time, random=~1|Subject, 
                  family=binomial, Dat)



# print the summary
summary(mod4glm)

# test these two model using Chi-Square test
anova(mod3glm,mod4glm, test="Chi")

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

