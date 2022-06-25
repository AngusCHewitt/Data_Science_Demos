

EWMA_fun <- function(df, length_n, lambda) {
  
  ##-- ewma fun ~ spec level of lambda
  ewma.func <- function(rets) {
    sig.p <- 0
    sig.s <- vapply(rets, function(r) sig.p <<- sig.p* lambda + (r^2)*(1 - lambda), 0)
    return(sqrt(sig.s))
  }
  
  
  if (length(df) < length_n) {
    ewma.func(df)
    
  }
  
  else {
    # turn ewma in a roling calc and window = n
    rolling_EWMA_n <- rollify(ewma.func, window = length_n, na_value = ewma.func(df))
    rolling_EWMA_n(df)
    
  }
}
