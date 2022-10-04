##-- Mixture model predictions of 2 different distrn using k = 2

data("NPreg", package = "flexmix")
mod <- flexmix(yn ~ x, data = NPreg, k = 2,
               model = list(FLXMRglm(yn ~ x, family= "gaussian")))

predict(mod, newdata = data.frame(x = 5))

ggplot(NPreg, aes(x = x, y = yn, color = class )) + geom_point()