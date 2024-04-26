library(carData)
library(effects)


summary(TitanicSurvival)

##-- fit glm mod prob of passenger surving
fit_mod <- glm(survived ~ sex + age + passengerClass, data = TitanicSurvival,
               family = "binomial")

##-- visualse all fixed effects in the mod
plot(predictorEffects(fit_mod))


