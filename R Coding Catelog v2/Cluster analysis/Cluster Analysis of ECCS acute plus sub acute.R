# Sub acute

# visualise distrn within regions and with hospitals

library(tidyverse) # tidy data
library(RcmdrMisc) # commandor packages
library(GGally) # paired visuals
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra) # multi-plot in a grid 
library(clValid) # dunn index fun
library(clustvarsel) # optimal var subset

# SAS dataset with VAED and ECCS Variables
Dataset <- readSAS("D:/Data integration/Databases/Cluster Analysis/add_complex.sas7bdat",
                   stringsAsFactors=TRUE, rownames=FALSE)

Dataset_WIES_Rank <- readXL("//internal.vic.gov.au/DHHS/HomeDirs6/ahew2812/Documents/Cluster Variables for Hosp Peer Groups.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="Variables", 
         stringsAsFactors=TRUE)


str(Dataset)

##-- Acute Dataset

# tidy the data to allow for easier visuals
Dataset %>%
  mutate(ECCS_Raw = as.character(ECCS_Raw)) %>%
  mutate(ECCS_Raw = as.numeric(ECCS_Raw)) %>%
  mutate(ECCS = as.numeric(ECCS_Raw)) %>%
  mutate(ECCS = round(as.numeric(ECCS),2)) -> Dataset

# nest d.s. at the hosp level with just care type cate and ECCS Raw
Dataset %>%
  select(CAMPUS,campname, Cate_Type_Cat, ECCS_Raw) %>%
  filter(Cate_Type_Cat != "Other") %>%
  group_by(CAMPUS,campname,Cate_Type_Cat) %>%
  nest() -> nest_ds_ECCS

# range f() 
range_fun <- function(df) {
  ECCS_range <- max(df$ECCS_Raw) -  min(df$ECCS_Raw)
  ECCS_range }

# sum no. esp f() 
volume_fun <- function(df) {
  Volumes <- nrow(df)
  Volumes }

# ECCS raw percentiles
percentile_fun <- function(df) {
  percentile_10 <- quantile(df$ECCS_Raw,.1)
  percentile_25 <- quantile(df$ECCS_Raw,.25)
  percentile_50 <- quantile(df$ECCS_Raw,.50)
  percentile_75 <- quantile(df$ECCS_Raw,.75)
  percentile_90 <- quantile(df$ECCS_Raw,.90)
  
  data.frame(percentile_10,percentile_25,percentile_50,percentile_75,percentile_90)}

# map big data descriptive variables - range, volume and quartiles
nest_ds_ECCS %>%
  mutate( ECCS_range = map(data, range_fun)) %>%
  mutate( Volumes = map(data, volume_fun)) %>%
  mutate( Percentiles = map(data, percentile_fun)) %>% 
  unnest(Percentiles, ECCS_range, Volumes, .drop = TRUE) %>% 
  gather("Var","Values",4:10) %>%
  mutate(Var = as.factor(Var)) %>%
  mutate(obs = 1:1631) %>%
  spread(key = Cate_Type_Cat, value = Values) %>%
  mutate(Acute = ifelse(is.na(Acute),0,Acute)) %>%
  mutate(`Sub-Acute` = ifelse(is.na(`Sub-Acute`),0,`Sub-Acute`)) %>%
  select(-obs) %>%
  gather("acuity","values",4:5) %>%
  arrange(CAMPUS,acuity) %>%
  mutate(label = str_c(Var,"_",acuity)) %>%
  select(CAMPUS,campname,label,values ) %>%
  mutate(obs = 1:3262) %>%
  spread(key = label, value = values, fill = 0 ) %>%
  group_by(CAMPUS,campname) %>%
  summarise(ECCS_percentile_10_Acute = sum(percentile_10_Acute), ECCS_percentile_25_Acute = sum(percentile_25_Acute),
            ECCS_percentile_50_Acute = sum(percentile_50_Acute), ECCS_percentile_75_Acute = sum(percentile_75_Acute),
            ECCS_percentile_90_Acute = sum(percentile_90_Acute), ECCS_percentile_10_Sub_Acute = sum(`percentile_10_Sub-Acute`),
            ECCS_percentile_25_Sub_Acute = sum(`percentile_25_Sub-Acute`), ECCS_percentile_50_Sub_Acute = sum(`percentile_50_Sub-Acute`),
            ECCS_percentile_75_Sub_Acute = sum(`percentile_75_Sub-Acute`), ECCS_percentile_90_Sub_Acute = sum(`percentile_90_Sub-Acute`),
            ECCS_range_Acute_sum = sum(ECCS_range_Acute), ECCS_range_Sub_Acute_sum = sum(`ECCS_range_Sub-Acute`),
            Volumes_Acute_sum = sum(Volumes_Acute), Volumes_Sub_Acute_sum = sum(`Volumes_Sub-Acute`)) -> spread_sub_features


# join sub_acute data and 
spread_sub_features %>% left_join(Dataset_WIES_Rank, by = c("CAMPUS" = "Campus")) -> Join_Ds_WIES_Rank


# scale all var and store in a d.f. object
Join_Ds_WIES_Rank %>%
  select(-campname, -Hosp.Name, -Campus_type ) -> tidy_join_d.s.


write.csv(Join_Ds_WIES_Rank,"percentile.csv")

getwd()

clust_ds <- tidy_join_d.s.[2:59]

# IQR range for acute and sub-acute episodes                                 
Acute_IQR <- spread_sub_features[c("ECCS_percentile_75_Acute")] - spread_sub_features[c("ECCS_percentile_25_Acute")]
Sub_Acute_IQR <- spread_sub_features[c("ECCS_percentile_75_Sub_Acute")] - spread_sub_features[c("ECCS_percentile_25_Sub_Acute")]
box_df <- data.frame(Acute_IQR,Sub_Acute_IQR)

# change colnames
names(box_df)[names(box_df) == "ECCS_percentile_75_Acute"] <- "Acute_IQR"
names(box_df)[names(box_df) == "ECCS_percentile_75_Sub_Acute"] <- "Sub_Acute_IQR"

# combine d.s.
combine_ds <- data.frame(clust_ds,box_df)

# row names need to be in a vector form
row.names(combine_ds) <- spread_sub_features$campname

# scale variables
scale_combine_ds <- data.frame(scale(combine_ds))

names(scale_combine_ds)

# select range and volume for acute and sub-acute
scale_comb_ds <- scale_combine_ds[c("Volumes_Acute_sum","ECCS_range_Acute_sum","Ave.WIES.Wt_All_Funded")]

# set seed as the cluster numbers and starthng locations are randomised
set.seed(678)

# function to compute total within-cluster sum of square 
wss_ratio <- function(k) {
  data = kmeans(scale_comb_ds, k, nstart = 20 )
  
  ratio = data$tot.withinss / data$totss
}

# Compute and plot wss for k = 1 to k = 10
k.values <- 1:10

# extract wss for 1-10 clusters
wss_ratio <- map_dbl(k.values, wss_ratio)

plot(wss_ratio,type = "o")

diff_data_acute = data.frame(actuals_wss = wss_ratio,difference =c(0, abs(diff( wss_ratio))))


set.seed(678)

# perform k means cluster analysis
clus_ds <- kmeans(sub_scale_ds, centers = 6, nstart = 20)

# Ratio WSS to TSS
clus_ds$tot.withinss / clus_ds$totss

# visualise clusters
(P1 <- fviz_cluster(clus_ds, data = scale_comb_ds,  geom = "point", main = "Clusters of Acute Episodes (all public Hospitals)"))

# add cluster to hospital list
clust_ds_export <- data.frame(sub_scale_ds, cluster = as.factor(clus_ds$cluster))

# output acute hospitals
write.csv(clust_ds_export,"acute_sub_cluster.csv")


getwd()




