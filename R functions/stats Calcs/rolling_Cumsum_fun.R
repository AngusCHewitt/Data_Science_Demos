rolling_Cumsum_fun <-  function(Vector, seq_Length) {
  # iterative rolling cumsum   
  
  output <- data.frame( Cumsum = 1:length(Vector)) #1 Output
  # return rolling cumsums 
  for (i in 1:length(Vector)) { # 2. sequence
    if (i == 1) { 
      output$Cumsum[[i]] = Vector[1]
    }
    
    else if (i <= seq_Length ) {
      output$Cumsum[[i]] = cumsum(Vector[1:i])[i]} # body
    
    else  { 
      j = i - seq_Length + 1
      output$Cumsum[[i]] = cumsum(Vector[j:i])[seq_Length]} # body
    
  }  
  {
    output$Cumsum # output quantile vector
  }
}

