##-- sclae vars using min max scaling
min_Max_scaled_Fun <- function(df) {
  
  data <- (df - min(df)) / (max(df) - min(df))
  data
  
}


##-- scales vars from 0 - 1, important for clustering with dist measures, i.e. kmans, pam and hclust, maintains distrn shape