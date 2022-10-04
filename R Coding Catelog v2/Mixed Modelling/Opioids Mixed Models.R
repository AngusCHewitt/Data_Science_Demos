library(gee)

Opioids_SF_Study <- read.csv("H:/My Documents/Opioids.csv")
View(Opioids_SF_Study)
str(Opioids_SF_Study)

## coerce int into factor variables
Opioids_SF_Study <- within(Opioids_SF_Study, {
  At_risk_prior <- as.factor(At_risk_prior)
  CLIB_prior <- as.factor(CLIB_prior)
  Gap_12mths <- as.factor(Gap_12mths)
  Interpreter <- as.factor(Interpreter)
  MH_prior <- as.factor(MH_prior)
  Opioids0 <- as.factor(Opioids0)
  Opioids1 <- as.factor(Opioids1)
  Opioids2 <- as.factor(Opioids2)
  Subjects <- as.factor(Subjects)
  Term_dev_prior <- as.factor(Term_dev_prior)
})

## reshape the data into a long form - varied by time
Dat <- reshape(Opioids_SF_Study, direction="long", 
               varying = c("Opioids1","Opioids2"),
               idvar  = c("Subjects","treatment","Capacity_month_prior","Interpreter","MH_prior",
                          "At_risk_prior","CLIB_prior","Gap_12mths","Opioids0","Term_dev_prior")
               ,sep="")

## change Dat col names
#colnames(Opioids_Long) <- c("Subject","Opioid","Sex","Age_At_Surg","Days_To_Surg","Year_of_Surg",
#                          "capacity_Month_Prior","Comp_3_Prior","Time","Opioids")

View(Dat)

# glm model for resp status
resp_glm1 <- glm(Opioids ~ Capacity_month_prior + treatment + Opioids0
                ,data = Dat, family = "binomial")

summary(resp_glm1)

# include time in the glm model
resp_glm2 <- glm(Opioids ~ Capacity_month_prior + treatment + Opioids0 + time 
                ,data = Dat, family = "binomial")


summary(resp_glm2)

# compare the 2 models goodness of fit to the data
anova(resp_glm1, resp_glm2, test="Chisq")

# mean estimated treatment cofficient - log odds
Mean_Estimate_Log <- summary(resp_glm1)$coefficients["treatment[T.surgery]",
                                                     "Estimate"]

# mean estimated treatment cofficient
Mean_Estimate <- exp(Mean_Estimate_Log)

Mean_Estimate

# standand error for treatment coefficient - log scale
se <- summary(resp_glm1)$coefficients["treatment[T.surgery]",
                                      "Std. Error"] 



# 95% confidence interval - Avg est odds of treatment effect
C.I    <-  exp(coef(resp_glm1)["treatment[T.surgery]"] +
                 c(-1, 1) * se * qnorm(0.975))


C.I  
