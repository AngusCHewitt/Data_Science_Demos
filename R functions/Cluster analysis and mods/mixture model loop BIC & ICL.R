##--- Loop through multiple mixtur models and store totao WSS and BSS in a list

#Loop car seasonal fun 

mixture_Mod_Loop_multi_K_fun <- function(Matrix_dt, rand_no, tol, k, nreps) {
 
    ##00 fun includes multiple levels of k and comapres BIC and ICl models WSS.BSS and seaosnal stablity acorss clsuetrs 
  ## k sep 2 or more values
  ##-- run models iteratively one at a time,
  ##-- need to adjust whether store the model objs if need (i.e. add to lsit at end fun)

  ##-- mixturemdodel
  set.seed(rand_no)
  mvpois_mix_model_optimal <- stepFlexmix(Matrix_dt ~ 1,
                                          k = k, 
                                          nrep = nreps, 
                                          model = FLXMCmvpois(),
                                          control = list(iter.max = 1000, tolerance = tol, classify = "w")) # tolerance plays big factor in model run time 
  
  # find the best fitted model according to the lowest BIC value
  BIC_optimal_Mod <- getModel(mvpois_mix_model_optimal, which = "BIC") 
  ICL_optimal_Mod <- getModel(mvpois_mix_model_optimal, which = "ICL") 
  
  
  ##-- store mixture mod with cluster stats
  list(BIC_Mod = BIC_optimal_Mod,
       ICL_Mod = ICL_optimal_Mod)
}


