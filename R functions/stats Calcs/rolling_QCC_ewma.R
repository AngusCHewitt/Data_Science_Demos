
rolling_EWMA_fun <-  function(Vector, seq_Length, Lambda) {
  require(qcc) # quality control stats 
  # iterative rolling ewma 
  
  ##-- spec rolling seq length and lambda (0-1)
  
  output <- data.frame( EWMA = 1:length(Vector),
                        ewma_centre = 1:length(Vector)) #1 Output
  # can return EWMA, cemtre, nsigma (calc control limits)
  for (i in 1:length(Vector)) { # 2. sequence
    if (i == 1) { 
      output$EWMA[[i]] = Vector[1]
      output$ewma_centre[[i]] = Vector[1]
    } 
    else if (i <= seq_Length ) {
      output$EWMA[[i]] = ewma(Vector[1:i], lambda = Lambda, plot = FALSE)$y[i]
      output$ewma_centre[[i]] = ewma(Vector[1:i], lambda = Lambda, plot = FALSE)$center} # body
    else  { 
      j = i - seq_Length + 1
      output$EWMA[[i]] = ewma(Vector[j:i], lambda = Lambda, plot = FALSE)$y[seq_Length]
      output$ewma_centre[[i]] = ewma(Vector[j:i], lambda = Lambda, plot = FALSE)$center} # body
    
  }  
  {output$EWMA}
  }
