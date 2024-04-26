# fit log distrn to -/+ numbers
log_Trans_fun <- function(x) {
    
  vec = c(1:length(x))
  for (i in seq_along(x)) {
    
    if (x[i] < 0) { vec[i] = -log(abs(x[i]+1))}
  
    else  {vec[i] = log(x[i]+1)}
  }
   return(vec)
}

