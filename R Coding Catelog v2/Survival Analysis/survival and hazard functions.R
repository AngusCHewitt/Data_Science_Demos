library(survival)
library(coin)

## Surv(time, event) ~ group 

# load glioma dataset
data("glioma", package = "coin")

# models survival functions


# survival curve for treatment Grade 3
g3 <- subset(glioma, histology == "Grade3")
plot(survfit(Surv(time, event) ~ group, data = g3),
main = "Grade III Glioma", lty = c(2, 1),
ylab = "Probability", xlab = "Survival Time in Months",
legend.text = c("Control", "Treated"),
legend.bty = "n")

fit <- survfit(Surv(time, event) ~ group, data = g3)

# change par layout
layout(matrix(1:2, ncol = 2)) 

# survival curve for treatment GBM
g4 <- subset(glioma, histology == "GBM")
plot(survfit(Surv(time, event) ~ group, data = g4),
main = "Grade IV Glioma", ylab = "Probability",
lty = c(2, 1), xlab = "Survival Time in Months",
xlim = c(0, max(glioma$time) * 1.05))

# test if their is a significant difference between the 2 groups 
## control vs treatment - survival curves -  testing the placebo effect

logrank_test(Surv(time, event) ~ group, data = g3,
distribution = "exact")

logrank_test(Surv(time, event) ~ group, data = g4,
distribution = "exact")

# Can also test the treatment affect of both treatment vs placebo
logrank_test(Surv(time, event) ~ group | histology,
data = glioma, distribution = approximate(B = 10000))

# load breast screen data and create a new objects
GBSG2 <- TH.data::GBSG2

# Cox Repgression model (prop hazards model)
GBSG2_coxph <- coxph(Surv(time, cens) ~ ., data = GBSG2)
summary(GBSG2_coxph)

plot(survfit(Surv(time, cens) ~ horTh, data = GBSG2),
lty = 1:2, mark.time = FALSE, ylab = "Probability",
xlab = "Survival Time in Days")
legend(250, 0.2, legend = c("yes", "no"), lty = c(2, 1),
title = "Hormonal Therapy", bty = "n")


# can test is variable exhibit significant variation over time (fixed or varying 
## cofficients)

GBSG2_zph <- cox.zph(GBSG2_coxph)
GBSG2_zph

# plot relationship between Age Beta(t)for age
plot(GBSG2_zph, var = "age")

# check models adequency - with the "residuals" function (goodness of fit)

layout(matrix(1:3, ncol = 3))

# plot the residual against each coefficent
res <- residuals(GBSG2_coxph)

plot(res ~ age, data = GBSG2, ylim = c(-2.5, 1.5),
pch = "^", ylab = "Martingale Residuals",col="red")
abline(h = 0, lty = 3)

plot(res ~ pnodes, data = GBSG2, ylim = c(-2.5, 1.5),
pch = "+", ylab = "",col="grey")
abline(h = 0, lty = 3)

plot(res ~ log(progrec), data = GBSG2, ylim = c(-2.5, 1.5),
pch = "*", ylab = "",col="blue")
abline(h = 0, lty = 3)




