##-- calc Sum wOBA rolling 4 day calc 
rolling_Sum_Calc_fun <-  function(Vector, seq_Length) {
  # iterative rolling cumsum   
  
  output <- data.frame(Sum = 1:length(Vector)) #1 Output
  # return rolling Sum 
  for (i in 1:length(Vector)) { # 2. sequence
    if (i == 1) { 
      output$Sum[[i]] = Vector[1] 
    }
    
    else if (i <= seq_Length ) {
      output$Sum[[i]] = sum(Vector[1:i])} # body
    
    else  { 
      j = i - seq_Length + 1
      output$Sum[[i]] = sum(Vector[j:i])} # body
    
  }  
  {
    output$Sum # output quantile vector
  }
  }
