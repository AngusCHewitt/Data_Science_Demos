## Find optimal cluster

library(clvalid) # kmeans 
library(GGally) # visual matrices
library(tidyverse) # tidy data
library(fpc) # cluster stats
library(clustvarsel) # optimal var subset

## Use multivariant cobvariance matrix to id outliers

library(robustbase)
library(car)

Dataset <- readXL("/Users/angushewitt/Desktop/Hosp Peer Groups stats.xlsx", 
                  rownames=TRUE, header=TRUE, na="", sheet="Sheet 1", stringsAsFactors=TRUE)

getwd()

hosp_data <- read.csv("Hosp Peer Groups stats.csv")

str(hosp_data)

##-- Clustervar to find optimal var subset --##

# 2 logical variable for example
hist(hosp_data$Percentage.Sub.Acute.Bed.Days,breaks = 5)

hist(hosp_data$Victorian.Population.Served.By.Hospital,breaks = 5)

# subset to 2 intereting vars
subset_hosp <- data.frame( Prop_Sub_Actute = df_scaled_hosp$Percentage.Sub.Acute.Bed.Days,
                          Vic_Pop_served = df_scaled_hosp$Victorian.Population.Served.By.Hospital)


row.names(add_rows_names) <- hosp_data[1]

# set rand seed
set.seed(321)

# perform optimal subset search
hosp_sub <- clustvarsel(subset_hosp[-1], G = 4:6)

# number of cluster
no_clusters <- hosp_sub$model["G"]

set.seed(828)

# run kmeans on entire d.s.
sub_km <- kmeans(subset_hosp[-1], centers = no_clusters$G, nstart = 20)


plot(subset_hosp[-1], col = sub_km$cluster)

##-- MV outiers --##


# take one subset using cluster groups = 1

Comb_clusters <- data.frame(subset_hosp, clusters = sub_km$cluster)

Comb_clusters %>%
  filter(clusters == 2) -> cluster_two

# scatterplot 
ggplot(data = cluster_two, aes(x = Prop_Sub_Actute, y = Vic_Pop_served)) + geom_point()

# Compute robust estimates for location and scatter
mcdresult <- covMcd(cluster_one[1:2])
robustcenter <- mcdresult$center
robustcov <- mcdresult$cov

# Add robust 97.5% tolerance ellipsoid
rad <- sqrt(qchisq(0.975, 2))
ellipse(center = robustcenter, shape = robustcov, radius = rad, col = "red")









