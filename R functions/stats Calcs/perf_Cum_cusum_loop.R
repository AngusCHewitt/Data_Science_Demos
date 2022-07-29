
##-- iterative cusum calc, return one calc sumation of all obs known prior to each game
cusum_fun <-  function(Vector, length) {
  ## return cum mean, pso and neg iterative loop over group var (i.e. per afl season)
  require(qcc) # perf stats package
  
  output <- data.frame( pos = 1:length(Vector),
                        neg = 1:length(Vector),
                        sd = 1:length(Vector), 
                        centre = 1:length(Vector)) #1 Output
  for (i in 1:length(Vector)) { # 2. sequence
    if (i == 1) { output$pos[[i]] = 0
    output$neg[[i]] = 0
    output$sd[[i]] = 0
    output$centre[[i]] = Vector[1]} 
    else { output$pos[[i]] = cusum(Vector[1:i], se.shift = .5,  plot = FALSE)$pos[i]
    output$neg[[i]] = cusum(Vector[1:i], se.shift = .5, plot = FALSE)$neg[i]
    output$sd[[i]] = cusum(Vector[1:i], se.shift = .5, plot = FALSE)$std.dev
    output$centre[[i]] = cusum(Vector[1:i],se.shift = .5, plot = FALSE)$center} # body
  }  
  {output}
  }
