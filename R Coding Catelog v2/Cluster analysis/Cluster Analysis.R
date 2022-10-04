##-------------------------- Profile clusters ----------------------##

load("/Users/angushewitt/Desktop/AFL Datasets/Pre 2020 Data/Step 4_AFL_tabs_dt_Career_Positions.Rdata") # load 2019 dataset



##-- pre 2020 seasons with 2020 cohort
List_of_DTs = list(AFL_Career_Positions, AFL_Career_Positions_2021)
AFL_Career_Positions_2021 = Reduce(function(...) merge(..., all = TRUE), List_of_DTs)


##-- add clusters to dataset
AFL_Career_Positions_2021 %>%
  mutate(Career_Cluster = clusters(Career_Best_Fitted_MixMod)) %>%
  mutate(Career_Cluster = as.factor(Career_Cluster)) -> AFL_Career_Positions_2021


##-- vie number of unique ID's which fall into each cluster each season
AFL_Career_Positions %>%
  group_by(Season, Career_Position) %>%
  summarise(counta = length(unique(ID))) %>%
  spread(Season, counta)

##-- vie number of unique ID's which fall into each cluster each season
AFL_Career_Positions %>%
  group_by( Career_Position) %>%
  summarise(counta = mean(TS_Running_Med_Goals)) 

AFL_Career_Positions_2021 %>%
  select(First.name, Surname, Career_Position) %>%
  View()



##-- tukey plot to comapre means of each factor levels (dual comparison)
plot(TukeyHSD(aov(HGS_Binary ~ cluster_fit_3 , data = AFL_Career_Positions)))

##-- summarise cluster stats
AFL_Career_Positions %>%
  group_by(cluster_fit_3) %>%
  summarise(mean(lagged_SOG)) 

##-- profile clusters
AFL_Career_Positions %>%
  ungroup() %>%
  select(First.name, Surname, TS_Running_Med_Goals, TS_Running_Med_Marks.Inside.50,cluster_fit_3, Season) %>%
  filter(cluster_fit_3 == 9) %>%
  View()

round(parameters(best_fit3),2)

##------------------------ Visualise clusters lambdas ----------------##
library(ggplot2)

# summary table with each components parameter values
param_pmm <- data.frame(round(parameters(best_fit),4))


param_pmm <- param_pmm %>% mutate(Type = colnames(gameday_stats))

# Visualise the Career_Custer
param_pmm %>% 
  gather(Components, Lambda, -Type) %>% 
  ggplot(aes(x = Type, y = Lambda, fill = Type)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ Components) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none") + coord_flip()

##------------------------------------ PCA visual ------------------------------------##

library(factoextra)
library(cluster)
##-- visulase cluster fit 
fviz_cluster(object = list(data = gameday_stats, cluster = clusters(best_fit1)), geom = "point")




##----------------------- mixed mod var and Approx fit --------------------------##


AFL_Career_Positions %>%
  dplyr::select(TS_Running_Med_Bounces, TS_Running_Med_Clearances,
                TS_Running_Med_Hit.Outs, TS_Running_Med_Inside.50s, TS_Running_Med_Marks_Out50,
                TS_Running_Med_Marks.Inside.50, TS_Running_Med_One.Percenters, TS_Running_Med_Rebounds,
                TS_Running_Med_SOG, TS_Running_Range_Bounces, TS_Running_Range_Clearances,
                TS_Running_Range_Hit.Outs, TS_Running_Range_Inside.50s, TS_Running_Range_Marks_Out50,
                TS_Running_Range_Marks.Inside.50, TS_Running_Range_One.Percenters, TS_Running_Range_Rebounds,
                TS_Running_Range_SOG) -> gameday_stats


dt_matrix <- as.matrix(gameday_stats)


# Set k to 15 for career position and Gameday produce consistent compariable peer groups 
set.seed(1001)
mvpois_mix_model <- stepFlexmix(dt_matrix ~ 1, 
                                k = 10:20, 
                                nrep = 10, 
                                model = FLXMCmvpois(),
                                control = list(iter.max = 1000, tolerance = 0.001, classify = "weighted"))

# find the best fitted model according to the lowest BIC value
#best_fit1 <- getModel(mvpois_mix_model, which = "ICL") # best fit was ICL
#best_fit2 <- getModel(mvpois_mix_model, which = "BIC") # best fit was BIC
#best_fit3 <- getModel(mvpois_mix_model, which = "BIC") #  best fit was BIC
#best_fit4 <- getModel(mvpois_mix_model, which = "BIC") # All same 
#best_fit5 <- getModel(mvpois_mix_model, which = "BIC") # All same 



param <- data.frame(parameters(best_fit1))


param <- param %>% mutate(Type = colnames(gameday_stats))

# reshape dataset to component to join lambda parameters   
AFL_Career_Positions %>%
  select(ID,Date, TS_Running_Med_Bounces, TS_Running_Med_Clearances,
         TS_Running_Med_Hit.Outs, TS_Running_Med_Inside.50s, TS_Running_Med_Marks_Out50,
         TS_Running_Med_Marks.Inside.50, TS_Running_Med_One.Percenters, TS_Running_Med_Rebounds,
         TS_Running_Med_SOG, TS_Running_Range_Bounces, TS_Running_Range_Clearances,
         TS_Running_Range_Hit.Outs, TS_Running_Range_Inside.50s, TS_Running_Range_Marks_Out50,
         TS_Running_Range_Marks.Inside.50, TS_Running_Range_One.Percenters, TS_Running_Range_Rebounds,
         TS_Running_Range_SOG) %>%
  mutate(Cluster_Components = clusters(best_fit1)) %>%
  gather("Variables","Values",-ID,-Date,-Cluster_Components) -> reshaped_Career_stats


# joined compoents paramters to each variable used in mod1
reshaped_Career_stats %>%
  left_join(param, by = c("Variables" = "Type")) -> reshaped_Career_stats


# Calc the goodness of fit for each campus/component/Var - combos
reshaped_Career_stats %>%
  mutate(sum_stand_diffs = case_when(Cluster_Components == 1 ~ ((Values + 1) - (Comp.1 + 1))/(Values + 1),
                                     Cluster_Components == 2 ~ ((Values + 1) - (Comp.2 + 1))/(Values + 1),
                                     Cluster_Components == 3 ~ ((Values + 1) - (Comp.3 + 1))/(Values + 1),
                                     Cluster_Components == 4 ~ ((Values + 1) - (Comp.4 + 1))/(Values + 1),
                                     Cluster_Components == 5 ~ ((Values + 1) - (Comp.5 + 1))/(Values + 1),
                                     Cluster_Components == 6 ~ ((Values + 1) - (Comp.6 + 1))/(Values + 1),
                                     Cluster_Components == 7 ~ ((Values + 1) - (Comp.7 + 1))/(Values + 1),
                                     Cluster_Components == 8 ~ ((Values + 1) - (Comp.8 + 1))/(Values + 1),
                                     Cluster_Components == 9 ~ ((Values + 1) - (Comp.9 + 1))/(Values + 1))) -> reshaped_Career_stats
#Cluster_Components == 10 ~ ((Values + 1) - (Comp.10 + 1))/(Values + 1))) 
#Cluster_Components == 11 ~ ((Values + 1) - (Comp.11 + 1))/(Values + 1))) 

# sum of abs differences
numSummary(reshaped_Career_stats[, "sum_stand_diffs", drop = FALSE], 
           statistics = c("mean", "sd", "IQR", "quantiles"), quantiles = c(0, 0.25, 0.5, 0.75, 1))



##-------------------- Kmeans cluster example -------------------------##

set.seed(1929)
frwd_clusters <- kmeans(player_stats, centers = 3,  nstart = 20 )

# set seed as the cluster numbers and starthng locations are randomised
set.seed(678)

# function to compute total within-cluster sum of square 
wss_ratio <- function(k) {
  data = kmeans(player_stats, k, nstart = 20 )
  
  ratio = data$tot.withinss / data$totss
}

# Compute and plot wss for k = 1 to k = 10
k.values <- 1:10

# extract wss for 1-10 clusters
wss_ratio <- map_dbl(k.values, wss_ratio)

plot(wss_ratio,type = "o")

CM_Subgroup %>%
  dplyr::select(
    TS_Running_Range_Bounces,
    TS_Running_Med_Goal.Assists) -> player_stats

set.seed(1818)
CM_clusters <- kmeans(player_stats, centers = 3, nstart = 20)



# merge datasets
List_of_DTs = list(CM_Subgroup, HB_Subgroup, Ruckman_Subgroup, Forward_Subgroup)
MergedDT = Reduce(function(...) merge(..., all = TRUE), List_of_DTs)

##-- join player sub categories
AFL_tabs_dt %>%
  left_join(MergedDT, by = c("ID", "Date")) -> AFL_tabs_dt

##-- add subcategory career positions



##--------------------------------- exclude obs not in vector --------------------------##

# edit not in data object function
'%!in%' <- function(x,y)!('%in%'(x,y))

# coul be another option
#'%out%' <- Negate(‘%in%’)


##----------------------------- merge object with same data stru (stacked) ------------##


# merge comp label datasets
List_of_DTs = list(comp_number,na_comp_number)
MergedDT = Reduce(function(...) merge(..., all = TRUE), List_of_DTs)



##------------------------------ Run parellel processing ----------------##

library(doParallel)
# change default 1 core to 3 core memory procoessing

#no_cores <- detectCores() - 1
#cl <- makeCluster(no_cores)  
#registerDoParallel(cl)
#start <- timestamp()

# Don't forget to stop the cluster when finished.
#stopImplicitCluster()
#rm(cl)
#end <- timestamp()


##------------------------------- Kmeans cluster loop -------------------##

# set seed as the cluster numbers and starthng locations are randomised
set.seed(678)

# function to compute total within-cluster sum of square 
wss_ratio <- function(k) {
  data = kmeans(data, k, nstart = 20 )
  
  ratio = data$tot.withinss / data$totss
}

# Compute and plot wss for k = 1 to k = 10
k.values <- 1:10

# extract wss for 1-10 clusters
wss_ratio <- map_dbl(k.values, wss_ratio)

plot(wss_ratio,type = "o")


##-------------------------------- Sample data for model testing -------##

# random sample pf game day data
mod_vars[-1] %>%
  sample_n(size = 1000, replace = F) -> AFL_Sample

GGally::ggpairs(AFL_Sample[13:26])

plot(density(AFL_Sample$TS_Running_Range_SOG))


AFL_tabs_dt %>%
  dplyr::filter(Career_Position == "Centre Mids") %>%
  select(TS_Running_Range_Bounces) %>%
  summary()

summary(as.factor(AFL_tabs_dt$Career_Position))

AFL_tabs_dt %>%
  select(First.name, Surname, TS_Running_Range_SOG, Career_Cluster,Season,Career_Position) %>%
  View()


#ggplot(data = AFL_tabs_dt, aes(x = as.factor(Career_Cluster), y = TS_Running_Med_Disposals)) + geom_boxplot()



##-------------------------------- Compare var densities facet wrap -------------------------------x-----##

# visulaise player vars which help distinguish the diff. player positions 
AFL_tabs_dt %>%
  ungroup() %>%
  select(Career_Bounces_PG, Career_Clearances_PG, Career_Contested.Marks_PG, Career_Hit.Outs_PG, Career_Mark_Inside_50_PG,
         Career_One.Percenters_PG, Career_SOG_PG, Career_Rebounds_PG) %>%
  gather("Var", "Values") -> positional_stats

positional_stats %>%
  ggplot(aes(x = Values, colours = Var )) + geom_density() + facet_wrap(~Var, scale = "free")

# gg-boxplots
ggplot(plot_dt, aes(x = Value ))  + facet_wrap(.~Var, scale = "free") + geom_histogram()


##------------------------------- PCA analysis find optimal level of compontents 


library(paran)
library(psych)


# examine the distn of current and fomer player across the PCA plain
summary(full_factomine_PCA)
plot(full_factomine_PCA,choix="ind",habillage=13)
dimdesc(full_factomine_PCA, axes = 1:2)
## To draw ellipses around the categories of the 13th variable (which is categorical)
plotellipses(full_factomine_PCA,23)


# Conduct a parallel analysis with paran().
air_paran <- paran(as.matrix(HO_Career_Pos_Cluster_full_mod[1:22]), seed = 1)

# Check out air_paran's suggested number of PCs to retain.
fa.parallel(HO_Career_Pos_Cluster_full_mod[1:22])

# Conduct a parallel analysis with fa.parallel().
air_fa_parallel <- fa.parallel(HO_Career_Pos_Cluster_full_mod[1:22])

# Check out air_fa_parallel's suggested number of PCs to retain.
air_fa_parallel$ncomp
# number of suggest componets was 4 




##------------------------------ New Player Code  -------------------------------##

##-- use abs difference of player stats agaianst the model parameter (min distance) to cluster them to appropriate component
##-- player subset used for model cross validation



##-- approx new player career position using model lambda paramters for each of the 10 components
# Fun loop through the difference of mod parameter (10 comps k = 10)
min_Diffs <- function(df) {
  
  # loop throu 10 comp
  minus_loop <- function(k) {
    
    # mixed mod paramters
    mod_param <- round(parameters(best_fit_itr_5),2)
    
    diffs = sum(abs((((df + 1) - (mod_param[,k]+1))/(mod_param[,k]+1))))}
  k <- 1:11
  min_scores <- map_dbl(k, minus_loop)}


##-- add unique code for each player / games played
AFL_tabs_dt %>%
  mutate(player_ID = str_c(ID,Date)) -> AFL_tabs_dt

AFL_Sample_itr_5 %>%
  mutate(player_ID = str_c(ID,Date)) -> AFL_Sample_itr_5

# edit not in data object function
'%!in%' <- function(x,y)!('%in%'(x,y))


##-- find each players running career position using approx
AFL_tabs_dt %>%
  dplyr::filter(player_ID %!in% AFL_Sample_itr_5$player_ID) %>%
  ungroup() %>%
  dplyr::select(player_ID,First.name, Surname, TS_Running_Med_Bounces, TS_Running_Med_Clearances,
                TS_Running_Med_Hit.Outs, TS_Running_Med_Inside.50s, TS_Running_Med_Marks_Out50,
                TS_Running_Med_Marks.Inside.50, TS_Running_Med_One.Percenters, TS_Running_Med_Rebounds,
                TS_Running_Med_SOG, TS_Running_Range_Bounces, TS_Running_Range_Clearances,
                TS_Running_Range_Hit.Outs, TS_Running_Range_Inside.50s, TS_Running_Range_Marks_Out50,
                TS_Running_Range_Marks.Inside.50, TS_Running_Range_One.Percenters, TS_Running_Range_Rebounds,
                TS_Running_Range_SOG) %>%
  group_by(player_ID,First.name, Surname) %>%
  nest() %>%
  mutate(player_clust = map(data, min_Diffs))%>%
  mutate(player_clust = map(player_clust, ~ data.frame(diffs = .,Career_Cluster = 1:11))) %>%
  mutate(min_diff = map_dbl(player_clust, ~ min(.$diffs))) %>%
  unnest(player_clust) %>%
  mutate(flag_min = if_else(min_diff == diffs,1,0)) %>%
  dplyr::filter(flag_min == 1) -> AFL_Appox_Clust



##-- Approx player cluster using sample cluster components --##

##-- remove sample players
AFL_tabs_dt %>%
  dplyr::filter(player_ID %!in% AFL_Sample_itr_5$player_ID) %>%
  select(-Career_Position) -> AFL_tabs_dt


##-- add approx career positions
AFL_Appox_Clust %>%
  dplyr::select(player_ID,Career_Position,Career_Cluster) %>%
  right_join(AFL_tabs_dt, by = "player_ID") -> AFL_tabs_dt


AFL_tabs_dt %>%
  ungroup() %>%
  mutate(Career_Position = case_when(Career_Cluster == 11 ~  "Half Back",
                                     Career_Cluster == 10 ~  "Key Forward",
                                     Career_Cluster == 9 ~   "Ruckman",
                                     Career_Cluster == 8 ~   "Frwd / PT Ruck",
                                     Career_Cluster == 7 ~   "Mid / Frwd Hybrid",
                                     Career_Cluster == 6 ~   "Forward",
                                     Career_Cluster == 5 ~   "Off Inside Mid",
                                     Career_Cluster == 4 ~   "Inexperienced",
                                     Career_Cluster == 3 ~   "Key Defender",
                                     Career_Cluster == 2 ~   "Defensive Forward",
                                     Career_Cluster == 1 ~   "Off Outside Mid")) -> AFL_tabs_dt


AFL_tabs_dt %>%
  select(First.name, Surname, Career_Cluster, Career_Position , Season, TS_Running_Med_SOG, TS_Running_Med_Marks.Inside.50) %>%
  filter(Career_Cluster == 5) %>%  
  View()



##-- approx new player career position using model lambda paramters for each of the 10 components
min_Diffs <- function(df) {
  
  # loop throu 10 comp
  minus_loop <- function(k) {
    
    # sum abs diffs obs - components as %
    mod_param <- round(parameters(Career_best_fit1_ICL),2) # paramters round to 2 decimal places
    diffs = sum(abs((df + 1) - (mod_param[,k] + 1))/(df + 1))}
  k <- 1:9
  min_scores <- map_dbl(k, minus_loop)}

##-- add unique code for each player / games played
AFL_Career_Positions_2021 %>%
  mutate(player_ID = str_c(ID,Date)) -> AFL_Career_Positions_2021

##-- find each players running career position using approx
AFL_Career_Positions_2021 %>%
  ungroup() %>%
  dplyr::select(player_ID,First.name, Surname, TS_Running_Med_Bounces, TS_Running_Med_Clearances,
                TS_Running_Med_Hit.Outs, TS_Running_Med_Inside.50s, TS_Running_Med_Marks_Out50,
                TS_Running_Med_Marks.Inside.50, TS_Running_Med_One.Percenters, TS_Running_Med_Rebounds,
                TS_Running_Med_SOG, TS_Running_Range_Bounces, TS_Running_Range_Clearances,
                TS_Running_Range_Hit.Outs, TS_Running_Range_Inside.50s, TS_Running_Range_Marks_Out50,
                TS_Running_Range_Marks.Inside.50, TS_Running_Range_One.Percenters, TS_Running_Range_Rebounds,
                TS_Running_Range_SOG) %>%
  group_by(player_ID,First.name, Surname) %>%
  nest() %>%
  mutate(player_clust = map(data, min_Diffs)) %>% # this part comp expensive
  mutate(player_clust = map(player_clust, ~ data.frame(diffs = .,Career_Cluster = 1:9))) %>% # insert col in player_clust with obs = 1:10
  mutate(min_diff = map_dbl(player_clust, ~ min(.$diffs))) %>%
  unnest(player_clust) %>%
  mutate(flag_min = if_else(min_diff == diffs,1,0)) %>%
  dplyr::filter(flag_min == 1) -> AFL_Appox_Clust


##-- add approx career positions
AFL_Appox_Clust %>%
  ungroup() %>%
  dplyr::select(player_ID,Career_Cluster) %>%
  right_join(AFL_Career_Positions_2021, by = "player_ID") -> AFL_Career_Positions_2021


##-- scale player TS stats and model centriods by each statistic 
reshaped_Career_stats %>%
  group_by(Stats) %>%
  nest() %>%
  mutate(Scale_Centriods =  map(data, ~data.frame(scale_Centriods = scale(.x$Centriods)))) %>%
  mutate(Scale_Actuals =  map(data, ~data.frame(scale_Actuals = scale(.x$Actuals)))) %>%
  unnest() -> Scaled_stats 

