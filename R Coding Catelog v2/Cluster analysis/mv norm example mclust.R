##-- mclust example for multivariant normal distns

##------------------------ GameDay Roles Pre 2019 ----------------------------------##
library(tidyverse) # tidy data
library(flexmix) # poisson mix mods
library(GGally) # matrix plots
library(mclust)

# Classification
data(banknote)
mod2 <- MclustDA(banknote[,2:7])
(mod2$class)
plot(mod2)
