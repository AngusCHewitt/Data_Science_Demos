
pam_Loop_fun <- function(dt, n1, n2, Stand = FALSE, Metric = "manhattan", ClusterOnly = FALSE,
                         NStart = 20)

{  
  ##-- pam cluster loop over multiple verion of K 
  ##-- n1 - first level of K in seq nd n2 is last i.e. 7,10 ~ &:10
  ##-- select over model params depend on the conext of the dt
  ##-- nstart no of rand position for mediods, 
  ##-- methods either manhattan or euclidean 
  require(cluster)
  
 asw <- list(0)
 ## Note that "k=1" won't work!
 for (k in n1:n2)
  asw[[k]] <-  pam(dt, k, metric = Metric,  stand = Stand, cluster.only = ClusterOnly, pamonce = 6, 
                   nstart = NStart)
 asw}


