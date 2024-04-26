
##-- iterative cusum calc, return one calc sumation of all obs known prior to each game
rolling_Cusum_fun <-  function(Vector, seq_Length, SE = 1L, std_Metric = "MR", Colnames) {
  require(qcc) # control charts
  ##-- spec rolling seq length, loop calcs i.e. calc cusm for 1 then 1 & 2, 1-4, 2-5 follow rolling seq
  ##-- Se rep the shift level pos/neg from process mean ?cusum 
  ##-- sttd dec metric can be "SD, or "MR", ?cusum for deatils
  ##-- lower teh se shift more sentive the senitive pos and neg to shift in mean
  ##-- MR more senstive to shifts in mean than SD
  ##-- if pos or neg a NaN value no deviation == 0
  
  output <- data.frame( pos = 1:length(Vector),
                        neg = 1:length(Vector),
                        sd = 1:length(Vector), 
                        cum_centre = 1:length(Vector)) #1 Output
  for (i in 1:length(Vector)) { # 2. sequence
    if (i == 1) { output$pos[[i]] = 0
    output$neg[[i]] = 0
    output$sd[[i]] = 0
    output$cum_centre[[i]] = Vector[1]} 
    else if (i <= seq_Length ) {
      output$pos[[i]] = cusum(Vector[1:i], se.shift = SE,  plot = FALSE,  std.dev = std_Metric)$pos[i] # cusum of 1st few games
      output$neg[[i]] = cusum(Vector[1:i], se.shift = SE, plot = FALSE, std.dev = std_Metric)$neg[i]
      output$sd[[i]] = cusum(Vector[1:i], se.shift = SE, plot = FALSE, std.dev = std_Metric)$std.dev
      output$cum_centre[[i]] = cusum(Vector[1:i],se.shift = SE, plot = FALSE, std.dev = std_Metric)$center} # body
    else  { 
      j = i - seq_Length + 1
      output$pos[[i]] = cusum(Vector[j:i], se.shift = SE,  plot = FALSE, std.dev = std_Metric)$pos[seq_Length] # rolling cumusm
      output$neg[[i]] = cusum(Vector[j:i], se.shift = SE, plot = FALSE, std.dev = std_Metric)$neg[seq_Length]
      output$sd[[i]] = cusum(Vector[j:i], se.shift = SE, plot = FALSE, std.dev = std_Metric)$std.dev
      output$cum_centre[[i]] = cusum(Vector[j:i],se.shift = SE, plot = FALSE,std.dev = std_Metric)$center} # body
  }  
  {
    ## outout df with cusum stats
    ##-- replce nan valuewith 0
    output %>%
      mutate(pos = if_else(is.nan(pos), 0, pos),
             neg = if_else(is.nan(neg), 0, neg)) -> output
    
    
    ##- rename cols to id var 
    colnames(output) <- c(str_c(Colnames,"_","pos"),
                          str_c(Colnames,"_","neg"),
                          str_c(Colnames,"_","sd"),
                          str_c(Colnames,"_","cum_centre"))
    
    output
  
  }
  }

