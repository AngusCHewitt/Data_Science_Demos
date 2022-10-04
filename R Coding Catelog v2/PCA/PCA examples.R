library(FactoMineR)
#factoextra::decathlon2

data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13)
## plot of the eigenvalues
## barplot(res.pca$eig[,1],main="Eigenvalues",names.arg=1:nrow(res.pca$eig))
summary(res.pca)
plot(res.pca,choix="ind",habillage=13)
dimdesc(res.pca, axes = 1:2)
## To draw ellipses around the categories of the 13th variable (which is categorical)
plotellipses(res.pca,13)

## Not run: 
## Graphical interface
require(Factoshiny)
res <- Factoshiny(decathlon)

## Example with missing data
## use package missMDA
##-- continuous vars
require(missMDA)
data(orange)
nb <- estim_ncpPCA(orange, ncp.min=0, ncp.max=5, method.cv="Kfold", nbsim=50)
imputed <- imputePCA(orange, ncp= nb$ncp)
res.pca <- PCA(imputed$completeObs)
