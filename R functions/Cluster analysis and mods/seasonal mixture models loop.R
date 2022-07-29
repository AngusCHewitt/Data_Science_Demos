##--- Loop through multiple mixtur models and store totao WSS and BSS in a list

#Loop car seasonal fun 

mixture_Mod_seasonl_Loop <- function(Matrix_dt,  season_vec, rand_no, tol, k, nreps) {
source("~/Desktop/R functions/Cluster analysis and mods/cluster stats & vis funs.R") #  cluster stats fund
  ##-- matix icnludes all cluster vars intested for the singular mod
  ##-- allowed multiple levels of k 
  ##-- radnom states seq of 5 numbers
  ##  seaoson vector , assigned vector to "season vec"
  ##-- use if ladder to choose the optimal model, too time consuming to comapred all WSS/BSS 
  ##-- when optima; var a decided upon rely more of BIC to find optimal mod
  ##--check for model stabliity over time
  ##00 get model best BIC models 


  ##-- pois ixture mod, 1 of 5
  set.seed(rand_no[1])
  mvpois_mix_model_optimal <- stepFlexmix(Matrix_dt ~ 1 | season_vec,
                                            k = k, 
                                            nrep = nreps, 
                                            model = FLXMCmvpois(),
                                            control = list(iter.max = 1000, tolerance = tol, classify = "w")) # tolerance plays big factor in model run time 
  
  mvpois_mix_model_optimal_1 <- getModel(mvpois_mix_model_optimal, which = "BIC") 
  
  
  set.seed(rand_no[2])
  mvpois_mix_model_optimal <- stepFlexmix(Matrix_dt ~ 1 | season_vec,
                                            k = k, 
                                            nrep = nreps, 
                                            model = FLXMCmvpois(),
                                            control = list(iter.max = 1000, tolerance = tol, classify = "w")) # tolerance plays big factor in model run time 
  
  mvpois_mix_model_optimal_2 <- getModel(mvpois_mix_model_optimal, which = "BIC") 
  
  set.seed(rand_no[3])
  mvpois_mix_model_optimal <- stepFlexmix(Matrix_dt ~ 1 | season_vec,
                                            k = k, 
                                            nrep = nreps, 
                                            model = FLXMCmvpois(),
                                            control = list(iter.max = 1000, tolerance = tol, classify = "w")) # tolerance plays big factor in model run time 
  
  mvpois_mix_model_optimal_3 <- getModel(mvpois_mix_model_optimal, which = "BIC") 
  
  
  set.seed(rand_no[4])
  mvpois_mix_model_optimal <- stepFlexmix(Matrix_dt ~ 1 | season_vec,
                                            k = k, 
                                            nrep = nreps, 
                                            model = FLXMCmvpois(),
                                            control = list(iter.max = 1000, tolerance = tol, classify = "w")) # tolerance plays big factor in model run ti
  
  mvpois_mix_model_optimal_4 <- getModel(mvpois_mix_model_optimal, which = "BIC") 
  
  set.seed(rand_no[5])
  mvpois_mix_model_optimal <- stepFlexmix(Matrix_dt ~ 1 | season_vec,
                                            k = k, 
                                            nrep = nreps, 
                                            model = FLXMCmvpois(),
                                            control = list(iter.max = 1000, tolerance = tol, classify = "w")) # tolerance plays big factor in model run time 
  
  mvpois_mix_model_optimal_5 <- getModel(mvpois_mix_model_optimal, which = "BIC") 
  
  
  ## find the min BIC amongst the 5 mixture mod iterations
  min_BIC <- min(BIC(mvpois_mix_model_optimal_1, mvpois_mix_model_optimal_2, mvpois_mix_model_optimal_3, 
                     mvpois_mix_model_optimal_4, mvpois_mix_model_optimal_5)$BIC)
  

##-- if else ladder to determine optimal BIC model, first ods
if (BIC(mvpois_mix_model_optimal_1) == min_BIC) {
  optimal_Mod <- mvpois_mix_model_optimal_1
} else if (BIC(mvpois_mix_model_optimal_2) == min_BIC) {
  optimal_Mod <- mvpois_mix_model_optimal_2
} else if (BIC(mvpois_mix_model_optimal_3) == min_BIC) {
  optimal_Mod <- mvpois_mix_model_optimal_3
} else if (BIC(mvpois_mix_model_optimal_4) == min_BIC) {
  optimal_Mod <- mvpois_mix_model_optimal_4
} else {optimal_Mod <- mvpois_mix_model_optimal_5}

## remvoe redundant vars
rm(mvpois_mix_model_optimal_1, mvpois_mix_model_optimal_2, mvpois_mix_model_optimal_3, mvpois_mix_model_optimal_4,
     mvpois_mix_model_optimal_5, mvpois_mix_model_optimal)

##--  new CV medium plus range to determine how much within summer squares variation  within each cluster
##-- WSS
Matrix_dt %>%
  data.frame() %>%
  mutate(clustering = clusters(optimal_Mod)) %>%
  WSS_clust_Var_level_Fun() -> optimal_Mod_WSS_cluster_Level


##-- BSS
Matrix_dt %>%
  data.frame() %>%
  mutate(clustering = clusters(optimal_Mod)) %>%
  BSS_clust_Var_fun() -> optimal_Mod_BSS_cluster_Level


##-- distn of cluster of seasons
Matrix_dt %>%
  data.frame() %>%
  mutate(clustering = clusters(optimal_Mod)) %>%
  mutate(Season = as.numeric(season_vec)) %>%
  group_by(clustering, Season) %>%
  summarise(cnt = n()) %>%
  spread(Season, cnt) -> optimal_Mod_seasonal_Distrn


##-- model stats
model_stats <- data.frame(BIC = BIC(optimal_Mod))

##-- remove moels
#rm(optimal_Mod, ICL_optimal_Mod)


##-- store mixture mod with cluster stats
list(BIC_WSS = optimal_Mod_WSS_cluster_Level,
     BIC_BSS = optimal_Mod_BSS_cluster_Level,
     BIC_seasonal= optimal_Mod_seasonal_Distrn,
     BIC_stats = model_stats,
     optimal_BIC_mod = optimal_Mod)
  }


