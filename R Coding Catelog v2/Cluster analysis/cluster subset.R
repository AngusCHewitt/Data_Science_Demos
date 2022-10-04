# loop to find optimal cluster subset

library(clvalid) # kmeans 
library(GGally) # visual matrices
library(tidyverse) # tidy data
library(fpc) # cluster stats
library(clustvarsel) # optimal var subset

# load wine data
data(wine, package='rattle')
head(wine)

str(wine)

# scale wine d.s.
scale_wine <- scale(wine[-1])
scale_wine <- as.data.frame(scale_wine)

##-- Clustervar to find optimal var subset --##


# visual plot
#ggpairs(wine)

set.seed(867)

# run kmeans on entire d.s.
wine_km <- kmeans(scale_wine, centers = 3, nstart = 20)

# review model accuarcy
full_cluster_tab <-  table(wine$Type,wine_km$cluster)

acc_full_mod <- 1-sum(diag(full_cluster_tab))/sum(full_cluster_tab)


#   1  2  3
# 1 13 46  0
# 2 20  1 50
# 3 29  0 19

# ratio WSS / TSS
(wine_km$tot.withinss / wine_km$totss) ## .1348

# distance between obs and centriods
dist_wine <- dist(wine[-1],method="euclidean" )

dunn_score <- dunn(dist_wine,clusters = wine_km$cluster, Data = wine[-1])


cluster.stats(dist_wine, wine_km$cluster)

#-- loop through kmeans cluster for every var in d.f. --#

result <- data.frame(matrix(nrow = length(wine[-1]), ncol = 3))
names <- vector("character",13)

# loop calc dunn index and WSS / TSS for each single variable kmeans clusters  
for (i in 1:length(wine[-1])) {
  
  set.seed(454)

  
    # kmeans k = 3 for each vector in d.f.
  wine_km <- kmeans(scale_wine[,i], centers = 3, nstart = 20)
  
  # dist measure
  dist_wine <- dist(scale_wine[,i],method="euclidean" )
  
  # cluster statistics for each obs
  wine_cluster_stats <- cluster.stats(dist_wine, wine_km$cluster)

  # Cluster stat vectors
  Av_Between <- wine_cluster_stats$average.between 
  Av_within <- wine_cluster_stats$average.within 
  Av_within_ss <- wine_cluster_stats$within.cluster.ss 
  dunn_1 <- wine_cluster_stats$dunn 
  dunn_2 <- wine_cluster_stats$dunn2 
  
  
  # loop in stats, var and  col names
  result[i,1] <-  names(scale_wine[i])
  result[i,2] <-  dunn_1
  result[i,3] <-  dunn_2
  result[i,4] <-  Av_within_ss
  result[i,5] <-  Av_within
  result[i,6] <-  Av_Between
  
  
  # add col names
  colnames(result) <- c("Var","dunn_1","dunn_2","Av_within_ss","Av_within","Av_Between")
  
  dataset = cbind(result, ranks = rank(result$dunn_1))
  
  dataset
}


# select top 5 and comapre with full model

plot(scale_wine$Phenols)

dataset %>%
  filter(ranks > 8) -> top_5

scale_wine %>%
  select("Phenols","Flavanoids","Proline") -> df_top_5

set.seed(676)

top_5_km <- kmeans(df_top_5,centers = 3)

# review model error rate (min.)
cluster_tab <-  table(wine$Type,top_5_km$cluster)

1-sum(diag(cluster_tab))/sum(cluster_tab)

# distance between obs and centriods
dist_wine_top_5 <- dist(df_top_5,method="euclidean" )

# cluster stats
cluster.stats(dist_wine_top_5, top_5_km$cluster)

# visualise scatter plot matrix with cluster set to col 
cluster_tab

# matrix
plot(df_top_5, col = top_5_km$cluster)

# individual obs
plot(scale_wine$Flavanoids, col = top_5_km$cluster)

# more comprehesive cluster stats
cqcluster.stats(dist_wine_top_5, top_5_km$cluster)

##-- Clustervar to find optimal var subset --##

# set rand seed
set.seed(321)

# perform optimal subset search
wine_optimal <- clustvarsel(scale_wine, G = 3)

# optimal var. selected
sub_names <- names(wine_optimal$subset)

# subset caled wine data to include subset var.
optimal_subset <- scale_wine[,sub_names]


set.seed(828)

# run kmeans on entire d.s.
wine_sub_km <- kmeans(optimal_subset, centers = 3, nstart = 20)

# review model accuarcy
sub_clusters_tab <-  table(wine$Type,wine_sub_km$cluster)

# error rate = 0.039.. 
1-sum(diag(sub_clusters_tab))/sum(sub_clusters_tab)


# distance between obs and centriods
dist_opt <- dist(optimal_subset,method="euclidean" )

# cluster stats
cluster.stats(dist_wine_top_5, top_5_km$cluster)

##-- Clustervar to find optimal var subset + choose number of clusters --##

# set rand seed
set.seed(1729)

# perform optimal subset search
wine_optimal_nocluster <- clustvarsel(scale_wine, G = 1:9)

# mod summary
summary(wine_optimal_nocluster)

# optimal no. of cluster given the optimal var. subset selected for the mod
wine_optimal_nocluster$model["G"]

# optimal var. selected + opt clusters
sub_names <- names(wine_optimal_nocluster$subset)

# subset caled wine data to include subset var.
optimal_subset_noclusters <- scale_wine[,sub_names]


set.seed(198)

# run kmeans on entire d.s.
wine_optclust_km <- kmeans(optimal_subset_noclusters, centers = 3, nstart = 20)

# review model accuarcy
opt_sub_clusters_tab <-  table(wine$Type,wine_optclust_km$cluster)

# error rate = 
1-sum(diag(opt_sub_clusters_tab))/sum(opt_sub_clusters_tab)

# distance between obs and centriods
dist_opt_noclust <- dist(wine_optimal_nocluster,method="euclidean" )

# cluster stats
cluster.stats(dist_opt_noclust, wine_optclust_km$cluster)
