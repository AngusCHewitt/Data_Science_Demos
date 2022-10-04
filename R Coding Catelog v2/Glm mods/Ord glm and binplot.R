library(vcd)
library(Mass)


## Simple model with no conditioning variables
art.mod0 <- glm(Improved > "None" ~ Age, data = Arthritis, family = binomial)

binreg_plot(art.mod0, "Arthritis Data")
binreg_plot(art.mod0, type = "link") ## logit scale

## one conditioning factor
art.mod1 <- update(art.mod0, . ~ . + Sex)
binreg_plot(art.mod1)
binreg_plot(art.mod1, legend = FALSE, labels = TRUE, xlim = c(20, 80))

## two conditioning factors
art.mod2 <- update(art.mod1, . ~ . + Treatment)
binreg_plot(art.mod2)
binreg_plot(art.mod2, subset = Sex == "Male") ## subsetting
