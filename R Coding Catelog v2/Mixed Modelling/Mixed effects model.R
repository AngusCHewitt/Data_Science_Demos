library("lme4")
library("gee")

## load dataset BtheB
data("BtheB", package = "HSAUR2")

## create a long form of the dataset to allow for longitudial analysis
BtheB$subject <- factor(rownames(BtheB))
nobs <- nrow(BtheB)
BtheB_long <- reshape(BtheB, idvar = "subject",
                         varying = c("bdi.2m", "bdi.3m", "bdi.5m", "bdi.8m"),
                         direction = "long")
BtheB_long$time <- rep(c(2, 3, 5, 8), rep(nobs, 4))
subset(BtheB_long, subject %in% c("1", "2", "3"))

## create boxplot comparing 2 different trestment types BPI scores over time
layout(matrix(1:2, nrow = 1))
ylim <- range(BtheB[,grep("bdi", names(BtheB))], na.rm = TRUE)
tau <- subset(BtheB, treatment == "TAU")[,grep("bdi", names(BtheB))]
boxplot(tau, main = "Treated as Usual", ylab = "BDI",
           xlab = "Time (in months)", names = c(0, 2, 3, 5, 8),ylim = ylim)
btheb <- subset(BtheB, treatment == "BtheB")[,grep("bdi", names(BtheB))]

boxplot(btheb, main = "Beat the Blues", ylab = "BDI",
           xlab = "Time (in months)", names = c(0, 2, 3, 5, 8),
           ylim = ylim)

## Glm model which does not incorp cross correlations within the data structure
## s.e. of covariates also need to be adjusted to account of this autocorrel 
BtheB_glm1 <- glm(bdi ~ bdi.pre + time + treatment + drug +
                    length, data = BtheB_long,
                    family=gaussian, na.action = na.omit)

summary(BtheB_glm1)


BtheB_glm2 <- glm(bdi ~ bdi.pre + time * treatment + drug +
                      length, data = BtheB_long,
                    family=gaussian, na.action = na.omit)

summary(BtheB_glm2)

# plot residuals
plot(BtheB_glm2)



## Mixed model with random effect incorp in intercept only
BtheB_lmer1 <- lmer(bdi ~ bdi.pre + time + treatment + drug +
                           length + (1 | subject), data = BtheB_long,
                           REML = FALSE, na.action = na.omit)

## Mixed model with random effect incorp in intercept & Slope
BtheB_lmer2 <- lmer(bdi ~ bdi.pre + time + treatment + drug +
                         length + (time | subject), data = BtheB_long,
                          REML = FALSE, na.action = na.omit)

## compare the 2 above models with chi ^2 test
anova(BtheB_lmer1, BtheB_lmer2)



## gee for non normal response var independant structure
BtheB_gee1 <- gee(bdi ~ bdi.pre + trt + length + drug,
                  data = BtheB_long, id = subject, family = gaussian,
                  corstr = "independence")

summary(BtheB_gee1)

## gee for non normal response var exchangeable structure
BtheB_gee2 <- gee(bdi ~ bdi.pre + trt + length + drug,
                  data = BtheB_long, id = subject, family = gaussian,
                  corstr = "exchangeable")

## compare the 2 above models with chi ^2 test
anova(BtheB_gee1, BtheB_gee2)



data("respiratory", package = "HSAUR2")
resp <- subset(respiratory, month > "0")
resp$baseline <- rep(subset(respiratory, month == "0")$status,
                        rep(4, 111))



