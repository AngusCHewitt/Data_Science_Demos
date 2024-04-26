
rolling_Median_fun <-  function(Vector, seq_Length) {
  # iterative rolling variance between median and quantile 
  
  output <- data.frame( Median = 1:length(Vector)) #1 Output
  # can return Median, cemtre, nsigma (calc control limits)
  for (i in 1:length(Vector)) { # 2. sequence
    if (i == 1) { 
      output$Median[[i]] = Vector[1]
    }
    else if (i <= seq_Length ) {
      output$Median[[i]] = median(Vector[1:i]) # allow for variance at 4th game
    } 
    else  { 
      j = i - seq_Length + 1
      output$Median[[i]] = median(Vector[j:i])
    } # body
    
  }  
  {
    output$Median # output quantile vector
  }
}




rolling_Var_med_quant_diff_fun <-  function(Vector, seq_Length, Quantile_no) {
  # iterative rolling variance between median and quantile 
  
  output <- data.frame( Median = 1:length(Vector),
                        Quantile = 1:length(Vector)) #1 Output
  # can return Median, cemtre, nsigma (calc control limits)
  for (i in 1:length(Vector)) { # 2. sequence
    if (i == 1) { 
      output$Median[[i]] = Vector[1]
      output$Quantile[[i]] = Vector[1]
    }
    else if (i < 4 ) {
      output$Median[[i]] = median(Vector[1:i]) # no various in first rolling seq
      output$Quantile[[i]] = median(Vector[1:i])} # body
    
    else if (i <= seq_Length ) {
      output$Median[[i]] = median(Vector[1:i]) # allow for variance at 4th game
      output$Quantile[[i]] = quantile(Vector[1:i],Quantile_no)} # body
    
    else  { 
      j = i - seq_Length + 1
      output$Median[[i]] = median(Vector[j:i])
      output$Quantile[[i]] = quantile(Vector[j:i], Quantile_no)} # body
    
  }  
  {
    output %>%
      mutate(Quantile = Quantile - Median) %>%
      select(Quantile) -> output #calc var between median and quantile
    
    output$Quantile # output quantile vector
  }
}


rolling_Var_quant_fun <-  function(Vector, seq_Length, Quantile_no) {
  # iterative rolling variance between median and quantile 
  
  output <- data.frame( Quantile = 1:length(Vector)) #1 Output
  # can return Median, cemtre, nsigma (calc control limits)
  for (i in 1:length(Vector)) { # 2. sequence
    if (i == 1) { 
      output$Quantile[[i]] = Vector[1]
    }
    else if (i < 3 ) {
      output$Quantile[[i]] = Vector[2]} # body
    
    else if (i <= seq_Length ) {
      output$Quantile[[i]] = quantile(Vector[1:i],Quantile_no)} # body
    
    else  { 
      j = i - seq_Length + 1
      output$Quantile[[i]] = quantile(Vector[j:i], Quantile_no)} # body
    
  }  
  {
    output$Quantile # output quantile vector
  }
}
