# Description: Detecting and avoiding copying

# copying objects is the #1 issue related to memory and performance

# Demonstrate the problem: rbind copies data.frames -------------------------------------------

aDataFrame <- data.frame(apogee = as.numeric(UCS_Satellite_Database$Apogee..km.),
                         perigee = as.numeric(UCS_Satellite_Database$Perigee..km.),
                         orbit = UCS_Satellite_Database$Class.of.Orbit)

object.size(aDataFrame) # ~39,352 bytes. Varies by system

tracemem(aDataFrame)

anotherDataFrame <-
    data.frame(
      apogee = runif(1, min = 300, max = 900),
      perigee = runif(1, min = 187, max = 900),
      orbit = sample(c("LEO", "GEO", "MEO"), 1)
    )
aDataFrame <- rbind(aDataFrame, anotherDataFrame)

# this allows us to watch an object being copied
# we potentially have 5 copies * 79,456 bytes = 397,280 bytes

untracemem(aDataFrame)

# use data.table instead of data.frame -------------------------------------------------

install.packages("data.table")
library(data.table)

aDataTable <- data.table(apogee = as.numeric(UCS_Satellite_Database$Apogee..km.),
                         perigee = as.numeric(UCS_Satellite_Database$Perigee..km.),
                         orbit = UCS_Satellite_Database$Class.of.Orbit)

tracemem(aDataTable)
object.size(aDataTable) # ~39,912 bytes

# add data to an existing row in aDataFrame
aDataTable[ 1, c("apogee", "perigee", "orbit")] <-
  data.table(apogee = runif(1, min = 300, max = 900),
    perigee = runif(1, min = 187, max = 900),
    orbit = sample(c("LEO", "GEO", "MEO"), 1)
  )
object.size(aDataTable) # ~39,912 bytes

untracemem(aDataFrame)

