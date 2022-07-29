
goodness_of_fit_test_fun <- function(df, type_Distrn, test_Method) {
  #Goodness-of-fit test for duscrete distribution
  ##-- ckeck parameter of vcd::goodness fun to choose the right distrn and test method
require(vcd)## loading vcd package
gf <- goodfit(df, type= type_Distrn, method= test_Method) 

##-- return mode sum of observed and fitted distrn
mod_summary <- summary(gf)

##-- plot obs vs theoritical distrn
plot(gf,main="Actual data vs Discrete distn")

}

  

