
first_Order_anova_Loop_fun <-  function(df) {
  # iterative rolling variance between median and quantile 
  
  cols = length(df)
  
  output <- tibble(var_Name = 1:cols, 
                   percentage_Var = 1:cols) #1 iterative number col in df
  
  ## fir glm model and calc first order deviance ttat for each explanatory var in model
  for (i in 2:cols) { # 2. sequence
    
    ##-- select the target x ~ y
    df %>%
      select(1,i) -> mod_subset
    
    
    ##-- glm fitted to every first order explanaotry var, adjust the response var
    fit_glm_mod <-  glm('.' ~ ., data = mod_subset, family = binomial(link = "logit"))
    
    output$percentage_Var[[i]] = round(((fit_glm_mod$null.deviance - deviance(fit_glm_mod)) / fit_glm_mod$null.deviance)*100,2) # % dev explain by first order var
    output$var_Name[[i]] = colnames(mod_subset[2]) # colnams of first order var
    
    
  }  # body
  {
    output[-1,] %>% # remove firts row with empty vector
      mutate(rank_Var = rank(desc(percentage_Var))) %>%
      arrange(rank_Var) # ranks from larget % desv to lowest
  }
}

