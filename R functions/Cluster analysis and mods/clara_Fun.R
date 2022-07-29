
clara_Loop_fun <- function(df, K) {
  
  ##-- load cluster library
  ##-- return result for different metrics calc diss
  ##-- comapre standised and non vs baseline with default params
  cluster_mod_trace_base_euc <- clara(df, k = K, stand = FALSE, trace = 0L, metric = "euclidean")
  cluster_mod_trace_sample_1_euc <- clara(df, k = K, stand = FALSE, samples = 1000, trace = 0L, metric = "euclidean")
  cluster_mod_trace_sample_2_euc <- clara(df, k = K, stand = FALSE, samples = 2000, trace = 0L, metric = "euclidean")
  cluster_mod_trace_sample_3_euc <- clara(df, k = K, stand = FALSE, samples = 3000, trace = 0L, metric = "euclidean")
  cluster_mod_trace_sample_4_euc <- clara(df, k = K, stand = FALSE, samples = 5000, trace = 0L, metric = "euclidean")
  
  
  ## find the min BIC amongst the 5 mixture mod iterations
  min_objective_fun <- min(cluster_mod_trace_base_euc$objective, cluster_mod_trace_sample_1_euc$objective,
                 cluster_mod_trace_sample_2_euc$objective, cluster_mod_trace_sample_3_euc$objective,
                 cluster_mod_trace_sample_4_euc$objective)
  
  ##-- if else ladder to determine optimal objective fun 
  if (cluster_mod_trace_base_euc$objective == min_objective_fun) {
    optimal_Mod <- cluster_mod_trace_base_euc
  } else if (cluster_mod_trace_sample_1_euc$objective == min_objective_fun) {
    optimal_Mod <- cluster_mod_trace_sample_1_euc
  } else if (cluster_mod_trace_sample_2_euc$objective == min_objective_fun) {
    optimal_Mod <- cluster_mod_trace_sample_2_euc
  } else if (cluster_mod_trace_sample_3_euc$objective == min_objective_fun) {
    optimal_Mod <- cluster_mod_trace_sample_3_euc
  } else {optimal_Mod <- cluster_mod_trace_sample_4_euc}

  }





