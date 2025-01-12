# log transformation they can handle positive and negative values
log_transform <- function(x) {
  sign(x) * log(abs(x) + 1)
}


#The arcsinh or inverse hyperbolic sine, 
#is a transformation that behaves similarly to log for large values of x, 
#but behaves linearly for small x.
arcsinh_trans<-function(x){
  log(x + sqrt(x^2 + 1))
}

