# Description: Using parallel processing with R

# a function to return the expected end of life of a satellite
ExpectedEOLSatellite <- function(ucsSatObs) {
  dangerRatio <- switch(
    as.character(ucsSatObs[8]),
    elliptical = .1,
    GEO = .25,
    LEO = .5,
    MEO = .75,
    ... = 1
  )
  expectedLife <- ifelse(is.na(ucsSatObs[20]), 15, as.integer(ucsSatObs[20]))
  adjustedLife <- as.integer(expectedLife * 365 * dangerRatio)
  EOLdate <- as.Date(ucsSatObs[[19]], "%m/%d/%Y") + adjustedLife
  return(EOLdate)
}

# standard version of apply -----------------------------------------------------------

apply(UCS_Satellite_Database, 1, ExpectedEOLSatellite)

# parallel version of apply ----------------------------------------------

library(parallel)

detectCores() # identifies number of available cores
myCluster <- makeCluster(detectCores())

parApply(cl = myCluster, UCS_Satellite_Database, 1, ExpectedEOLSatellite)

# benchmark results -------------------------------------------------------

install.packages("microbenchmark")
library(microbenchmark)

benchmarkresults <- microbenchmark(
  original = apply(UCS_Satellite_Database, 1, ExpectedEOLSatellite),
  parallel = parApply(cl = myCluster, UCS_Satellite_Database, 1, ExpectedEOLSatellite)
)

boxplot(benchmarkresults) # it takes LONGER to parallel process
