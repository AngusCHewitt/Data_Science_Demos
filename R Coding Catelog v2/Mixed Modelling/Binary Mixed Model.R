## Page 238 Analysising Longtudinal data - GEE Models


library(gee)

#creating dataset respiratory
data("respiratory", package = "HSAUR2")

resp <- subset(respiratory, month > "0")

str(resp)

write.table(resp,"resp.csv",sep=",")
write.table(resp,"respiratory.csv",sep=",")



View(respiratory)

# create a variable for baseline resp status
resp$baseline <- rep(subset(respiratory, month == "0")$status,
                        + rep(4, 111))

# create a binary variable for resp status
resp$nstat <- as.numeric(resp$status == "good")

# numbers for Resp month   
resp$month <- resp$month[, drop = TRUE]

# glm model for resp status
resp_glm <- glm(status ~ centre + treatment + gender + baseline + age, 
                    data = resp, family = "binomial")

summary(resp_glm)

# gee model for resp status with indep structure
resp_gee1 <- gee(nstat ~ centre + treatment + gender + baseline + age, 
                    data = resp, family = "binomial", id = subject,
                    corstr = "independence", scale.fix = TRUE,
                    scale.value = 1)

summary(resp_gee1)

# gee model for resp status with exchangable structure - 
resp_gee2 <- gee(nstat ~ centre + treatment + gender + baseline +  age, 
                    data = resp, family = "binomial", id = subject,
                    corstr = "exchangeable", scale.fix = TRUE,
                    scale.value = 1)

summary(resp_gee2)

# mean estimated treatment cofficient - log odds
Mean_Estimate_Log <- summary(resp_gee2)$coefficients["treatment[T.treatment]",
                                      "Estimate"]

# mean estimated treatment cofficient
Mean_Estimate <- exp(Mean_Estimate_Log)

# standand error for treatment coefficient - log scale
se <- summary(resp_gee2)$coefficients["treatment[T.treatment]",
                                     "Robust S.E."] 


# 95% confidence interval - odds of treatment effect in log scale
C.I    <-  coef(resp_gee2)["treatment[T.treatment]"] +
            c(-1, 1) * se * qnorm(0.975)

# 95% confidence interval - Avg est odds of treatment effect
C.I    <-  exp(coef(resp_gee2)["treatment[T.treatment]"] +
            c(-1, 1) * se * qnorm(0.975))


