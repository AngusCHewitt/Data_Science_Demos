##--loop through  mixture model with 1 Level of k, find the model woth the lowest BIC

#Loop car seasonal fun 

Mixture_mod_Loop_fun <- function(tb, rand_No_seq, tol, k, nreps) {
 
  ##-- expecting data to be tibble of data frame, for the nest ts loops
  ##-- specofy seq of K > 2
  ##-- spec level k  want to return char var. 
  ## rand number need to be a sqe of 5 numbers,
  ##-- generate and compares 5 mixture models and selct one with the min BIC
  ##== reutrn WSS, =BSS along with moel obj/BIC stats
  ##-- max iterations set at 1000 and classifier = "w"
  
  ##-- convert tb to df then maatrix
  tb %>%
    data.frame() %>%
    as.matrix() -> Matrix_dt
  
  ##-- pois ixture mod, 1 of 5
  set.seed(rand_No_seq[1])
  mvpois_mix_model_optimal <- stepFlexmix(Matrix_dt ~ 1,
                                          k = k, 
                                          nrep = nreps, 
                                          model = FLXMCmvpois(),
                                          control = list(iter.max = 1000, tolerance = tol, classify = "w")) # tolerance plays big factor in model run time 
  
  mvpois_mix_model_optimal_1 <- getModel(mvpois_mix_model_optimal, which = "BIC") 
  
  
  
  set.seed(rand_No_seq[2])
  mvpois_mix_model_optimal <- stepFlexmix(Matrix_dt ~ 1,
                                            k = k, 
                                            nrep = nreps, 
                                            model = FLXMCmvpois(),
                                            control = list(iter.max = 1000, tolerance = tol, classify = "w")) # tolerance plays big factor in model run time 
  
  mvpois_mix_model_optimal_2 <- getModel(mvpois_mix_model_optimal, which = "BIC") 
  
  
  set.seed(rand_No_seq[3])
  mvpois_mix_model_optimal <- stepFlexmix(Matrix_dt ~ 1,
                                            k = k, 
                                            nrep = nreps, 
                                            model = FLXMCmvpois(),
                                            control = list(iter.max = 1000, tolerance = tol, classify = "w")) # tolerance plays big factor in model run time 
  
  mvpois_mix_model_optimal_3 <- getModel(mvpois_mix_model_optimal, which = "BIC") 
  
  
  set.seed(rand_No_seq[4])
  mvpois_mix_model_optimal <- stepFlexmix(Matrix_dt ~ 1,
                                            k = k, 
                                            nrep = nreps, 
                                            model = FLXMCmvpois(),
                                            control = list(iter.max = 1000, tolerance = tol, classify = "w")) # tolerance plays big factor in model run time 
  
  mvpois_mix_model_optimal_4 <- getModel(mvpois_mix_model_optimal, which = "BIC") 
  
  
  set.seed(rand_No_seq[5])
  mvpois_mix_model_optimal <- stepFlexmix(Matrix_dt ~ 1,
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
  

  ##--remove redundant objs
  rm(mvpois_mix_model_optimal_1, mvpois_mix_model_optimal_2, mvpois_mix_model_optimal_3, mvpois_mix_model_optimal_4,
     mvpois_mix_model_optimal_5, mvpois_mix_model_optimal)
  
  optimal_Mod
}


