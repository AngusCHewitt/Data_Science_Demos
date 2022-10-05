#-- example of a loop fun

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



##-- iterative cusum calc, return one calc sumation of all obs known prior to each game
cusum_fun <-  function(df) {
  output <- data.frame(pos = 1:nrow(df),
                       neg = 1:nrow(df),
                       sd = 1:nrow(df), 
                       centre = 1:nrow(df)) #1 Output
  for (i in 1:nrow(df)) { # 2. sequence
    if (i == 1) { output$pos[[i]] = 0
    output$neg[[i]] = 0
    output$sd[[i]] = 0
    output$sd[[i]] = df$lagged_Goal_scoring_Fatasy_scores[1]} 
    else { output$pos[[i]] = cusum(df$lagged_Goal_scoring_Fatasy_scores[1:i], plot = FALSE)$pos[i]
    output$neg[[i]] = cusum(df$lagged_Goal_scoring_Fatasy_scores[1:i], plot = FALSE)$neg[i]
    output$sd[[i]] = cusum(df$lagged_Goal_scoring_Fatasy_scores[1:i], plot = FALSE)$std.dev
    output$centre[[i]] = cusum(df$lagged_Goal_scoring_Fatasy_scores[1:i], plot = FALSE)$center} # body
  }  
  {output}
}

