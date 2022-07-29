
pam_Loop_fun <- function(dt, n1, n2, Stand = FALSE, Metric = "euclidean", ClusterOnly = FALSE)

{  
  ##-- pam cluster loop over multiple verion of K 
  ##-- n1 - first level of K in seq nd n2 is last i.e. 7,10 ~ &:10
  ##-- select over model params depend on the conext of the dt
  
 asw <- list(0)
 ## Note that "k=1" won't work!
 for (k in n1:n2)
  asw[[k]] <-  pam(dt, k, metric = Metric,  stand = Stand, cluster.only = ClusterOnly, pamonce = 6)
 asw}


