##-- Weighted MA of player Goals over the past 4 weeks (form guide)
Weighted_Goals_fun <-  function(df) {
  output <- data.frame(lagged_weighted_goals = 1:nrow(df)) #1 Output
  for (i in 1:nrow(df)) { # 2. sequence
    if (i == 1) { output$lagged_weighted_goals[[i]] = df$lagged_Goals[i] } 
    else if (i < 5) { output$lagged_weighted_goals[[i]] = mean(df$lagged_Goals[1:i], na.rm = TRUE) } 
    else { output$lagged_weighted_goals[[i]] =  sum(df$lagged_Goals[i] * .6,
                                                    df$lagged_Goals[i-1] * .2,
                                                    df$lagged_Goals[i-2] * .1,
                                                    df$lagged_Goals[i-3] * .1)} # 3. Body
  }  
  {output}
}
