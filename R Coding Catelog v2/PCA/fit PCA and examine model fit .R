data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13)
## plot of the eigenvalues
## barplot(res.pca$eig[,1],main="Eigenvalues",names.arg=1:nrow(res.pca$eig))
plot(res.pca)
plot(res.pca,choix="ind",habillage=13)
dimdesc(res.pca, axes = 1:2)
## To draw ellipses around the categories of the 13th variable (which is categorical)
plotellipses(res.pca,13)

##-- PCA biplot, vars and obs
fviz_pca(res.pca)

##-- PCA biplot, vars
fviz_pca_var(res.pca)

##-- contribution of each var to each dim
fviz_contrib(res.pca, "var")


##-- eigenvalues for each dims
fviz_eig(res.pca, "eigenvalue")
