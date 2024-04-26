
##-- calc Sum wOBA rolling 4 day calc 
rolling_Sum_ratio_Calc_fun <-  function(Vector_A, Vector_B, seq_Length) {
  # iterative rolling cumsum   
  
  output <- data.frame(Sum = 1:length(Vector_A)) #1 Output
  # return rolling Sum 
  for (i in 1:length(Vector_A)) { # 2. sequence
    if (i == 1) { 
      output$Sum[[i]] = Vector_A[1] / Vector_B[1]
    }
    
    else if (i <= seq_Length ) {
      output$Sum[[i]] = sum(Vector_A[1:i]) / sum(Vector_B[1:i])} # body
    
    else  { 
      j = i - seq_Length + 1
      output$Sum[[i]] = sum(Vector_A[j:i]) / sum(Vector_B[j:i])} # body
    
  }  
  {
    output$Sum # output quantile vector
  }
}
