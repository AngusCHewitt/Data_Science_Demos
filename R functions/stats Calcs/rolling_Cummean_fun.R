rolling_Cummean_fun <-  function(Vector, seq_Length) {
  # iterative rolling cummean   
  
  output <- data.frame( Cummean = 1:length(Vector)) #1 Output
  # return rolling cummeans 
  for (i in 1:length(Vector)) { # 2. sequence
    if (i == 1) { 
      output$Cummean[[i]] = Vector[1]
    }
    
    else if (i <= seq_Length ) {
      output$Cummean[[i]] = mean(Vector[1:i])} # body
    
    else  { 
      j = i - seq_Length + 1
      output$Cummean[[i]] = mean(Vector[j:i])} # body
    
  }  
  {
    output$Cummean # output quantile vector
  }
}

