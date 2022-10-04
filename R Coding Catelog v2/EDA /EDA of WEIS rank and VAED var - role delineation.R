# explore WIES rank sheet

# possibly use in mixed mod and EDA

library(tidyverse) # tidy data
library(GGally) # paired visuals
library(RcmdrMisc) # commander packages
library(clvalid) # dunn test 
library(fpc) # cluster stats
library(clustvarsel) # optimal var subset
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra) # multi-plot in a grid 
library(corrplot) # corrletions plot


Dataset <- readXL("//internal.vic.gov.au/DHHS/HomeDirs6/ahew2812/Documents/Cluster Variables for Hosp Peer Groups.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="Variables",stringsAsFactors=TRUE)

str(Dataset)

# remove cate vars
Dataset %>%
  select(-Campus,-Campus_type,-Hosp.Name) -> nums_ds

# feature no. zeros in each dataset
# function count number zeros
count_zeros <- function(df) {
  
  zeros <- sum(df$Value == 0)
  zeros
}

# nest d.s. by col names
nums_ds %>%
  gather("Var","Value") %>%
  group_by(Var) %>%
  nest() -> nest_ds

# add zero count
nest_ds %>%
  mutate(zero_cnt = map_int(data,count_zeros)) -> add_zeros_ds


# add cv - unitless meaure of variance, % of mean
cv_fun <- function(df) {
  cv <- sd(df$Value)/ 
    mean(df$Value)
}


# add cv to nest ds

add_zeros_ds %>%
  mutate(cv = map_dbl(data,cv_fun)) %>%
  mutate(obs = 1:44) -> add_cv_ds



# visualise the no of vars with excess zeros
ggplot(data = add_cv_ds, aes(x= zero_cnt, fill = zero_cnt > 100)) + geom_histogram() + ggtitle("Count of zero values for each Variables")


# exclude var > 100 zeros
add_cv_ds %>% 
  filter(zero_cnt < 100) -> excess_zero_excl


# scale vars to examine distrn (variability, str)
# scale d.s. -> coerce to dataframe
scaled_fun <- function(df) {
  scaled_dat <- scale(df)
  scaled_dat <- data.frame(scaled_dat)
  scaled_dat}


# add var. scaled
excess_zero_excl %>%
  mutate(scaled_data = purrr::map(data,scaled_fun)) %>%
  select(-obs) -> add_scaled_ds

# review var distn > 50 zeros 
add_scaled_ds %>%
  filter(zero_cnt > 50) %>%
  unnest(scaled_data, .drop = TRUE) %>%
  mutate(hosp_no = rep(1:146, each = 1, times = 5)) %>%
  select(hosp_no, Var, Value) -> zero_50_var
  #spread(Var,Value) -> zero_50_var

# visualise vars with 50 to 100 zeros
ggplot(data = zero_50_var) + geom_histogram(aes(Value)) + facet_wrap( ~ Var)

# spread hosp as rows and measure cols (two var exlude based on lack of variance)
add_scaled_ds %>%
  filter(Var != "Seps_Multi_Stay_Surg") %>%
  filter(Var != "W24.WIES_Multi_Stay_Surg") %>%
  unnest(scaled_data, .drop = TRUE) %>%
  mutate(hosp_no = rep(1:146, each = 1, times = 27)) %>%
  select(Var, Value, hosp_no) %>%
  spread(Var,Value) -> just_scaled_ds


# scaled correlations
scaled_cor <- cor(just_scaled_ds[2:28])

correl_plot <- corrplot(scaled_cor)


# correlations matrix with color densities
corrplot(scaled_cor,title = "Correlation Plot", method = "square", outline = T, 
         addgrid.col = "darkgray", order="hclust", diag = FALSE)


# spread hosp as rows and measure cols (two var exlude based on lack of variance)
add_scaled_ds %>%
  filter(Var != "Seps_Multi_Stay_Surg") %>%
  filter(Var != "W24.WIES_Multi_Stay_Surg") %>%
  unnest(data, .drop = TRUE) %>%
  mutate(hosp_no = rep(1:146, each = 1, times = 27)) %>%
  select(Var, Value, hosp_no) %>%
  spread(Var,Value) -> raw_dat_ds

# add row names 
row.names(raw_dat_ds) <- Dataset$Hosp.Name


##-- Use PCA to reduce the number of d.s. demensions --##
scaled_pca <- prcomp(raw_dat_ds[2:28],center = TRUE,scale. = TRUE)

summary(scaled_pca)

df_scaled <- data.frame(scaled_pca$rotation) 

ggplot(data = df_scaled, aes(x = PC1, y = PC2)) + geom_point()


##-- Use cluster analysis to determine highly correleated var groups

# spread hosp as rows and measure cols (two var exlude based on lack of variance)
add_scaled_ds %>%
  filter(Var != "Seps_Multi_Stay_Surg") %>%
  filter(Var != "W24.WIES_Multi_Stay_Surg") -> reduced_ds
  
reduced_ds %>%
  unnest(scaled_data, .drop = TRUE) %>%
  mutate(hosp_no = rep(1:146, each = 1, times = 27)) %>%
  select(Var, Value, hosp_no) %>%
  spread(Var,Value) -> scaled_dat_ds

# corrlations matrix for reduced varibales
scaled_dat_ds[2:28] %>%
  cor() -> reduced_cors

# visualise the WSS for each no. of K
wss <- 0
ratio <- 0 

# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(reduced_cors, centers = i, nstart = 20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
  ratio[i] <- km.out$tot.withinss / km.out$totss
}

# visualise the difference in WSS ratio for each cluster
clusters = 1:15
abs_diffs_WSS_Ratio = c(1,abs(diff(ratio)))

df_diffs = data.frame(clusters,abs_diffs_WSS_Ratio)

# plot WSS ratio lag 1 diffs, look for deminishing returns
lattice::bwplot(clusters ~ abs_diffs_WSS_Ratio , data = df_diffs)

# Plot total within sum of squares vs. number of clusters
plot(1:15, ratio, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares",
     main = "optimal level of clusters")


# 6 cluster account for enough of the variance
set.seed(1656)

cor_clusters_km <- kmeans(reduced_cors, centers = 7, nstart  = 20)


# visualise corrlations clusters
(P3 <- fviz_cluster(cor_clusters_km, data = reduced_cors,  geom = "point", palette = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d'), main = "Clusters of Standardised Variables") + theme_classic()) 


# add corrations matix 

# coerce into d.f. 
dat_cluster <- data.frame(cor_clusters_km$cluster)


dat_cluster %>%
  mutate(Var = row.names(dat_cluster)) -> add_var_nms

write.csv(add_var_nms,"cluster var.csv")


# add names to reduced correls
reduced_cors %>%
  data.frame() %>%
  mutate(Var = row.names(dat_cluster)) -> add_var_nms_cors


# join var cluster to unnested correlations d.s. 
add_var_nms %>% 
  left_join(add_var_nms_cors, by = "Var") -> add_clust_names_cors 

# review cluster correlations
add_clust_names_cors %>%
filter(cor_clusters_km.cluster == 7) -> cluster_7

cluster_7 %>%
select(cluster_7$Var,Var) %>%
as.tibble() -> Cluster_7_cols
Cluster_7_cols

# Correlations matrix with color densities
corrplot(Cluster_6_cols[1:5], is.corr = FALSE)


# Join var cluster to nested d.s.
reduced_ds %>% 
  left_join(add_var_nms, by = "Var") -> add_clust 


# Examine the distn to view each cluster var. distrn and dat str.

# Cluster 1

add_clust %>% 
  filter(cor_clusters_km.cluster == 6) %>%
  unnest(scaled_data, .drop = TRUE) -> cluster_6

p <- ggplot(data = cluster_6, aes(x = Var , y = abs(Value))) 

p + geom_jitter(alpha = 0.5) + geom_point(aes(x = Var , y = cv), size = 3, color = "red")+ coord_flip()


# select variables for each clsuter minus cluster 4 (only 2 variables with little var.)

# add uncorrel var flag
add_clust %>%
  mutate(uncorrel_vars = case_when(Var == "ALOS_Multi_Stay_Surg" ~ 1,
                                   Var == "Ave.WIES.Wt_All_Funded" ~ 1,
                                   Var == "Ave.WIES.Wt_Multi_Stay_Surg" ~ 1,
                                   Var == "No.of.CRGs.with_gr.or.eq_10.Seps" ~ 1,
                                   Var == "Percentage.Sub.Acute.Bed.Days" ~ 1,
                                   Var == "Prop.Top.10.DRG.Seps.Minus.Renal" ~ 1,
                                   Var == "W24.WIES_Multi_Stay" ~ 1,
                                   TRUE ~  0)) -> add_correl_vars
  
# uncorrel var only
add_correl_vars %>% 
  filter (uncorrel_vars == 1) %>% 
  unnest(scaled_data, .drop= TRUE) %>%
  select(Var, Value) %>% 
  mutate(hosp_no = rep(1:146, each = 1, times = 7)) %>%
  spread(Var,Value) -> spread_uncor_ds
  

names(spread_uncor_ds[2:8])




