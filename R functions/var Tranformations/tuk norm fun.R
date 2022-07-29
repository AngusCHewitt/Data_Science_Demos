##-- trabsfrom var to ND using tukey power trasnform, notwell suited to big data
tukey_ND_trans_Fun <- function(df, plot_TRUE = FALSE) {

##-- tukey transform  
library(rcompanion)
  T_tuk = transformTukey(df,
                         plotit=plot_TRUE,quiet = T,returnLambda = T)
  
  ##== asigneed trans data to obj
  tuk_Trans_data <- df^T_tuk
  tuk_Trans_data
  
  if(T_tuk == 1) {"warning lambda == 1"}
  else {tuk_Trans_data}
  }

