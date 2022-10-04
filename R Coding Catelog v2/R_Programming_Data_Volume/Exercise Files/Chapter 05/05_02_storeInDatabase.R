# Compare RAM access vs file access vs sql access 

# Be sure to load the sample data.frame UCS_Satellite_Database. 

# access from a data.frame -------------------------------------------------------
randomMedianDF <- function() {
  median(sample(UCS_Satellite_Database$Launch.Mass..kg.., size = 100), na.rm = TRUE)
}

# access from file on disk ------------------------------------------------
# install.packages("readr")
library(readr)

randomMedianFile <- function() {
  SatDat <- read_tsv("../UCS_Satellite_Database" )
  median(sample(SatDat$`Launch Mass (kg.)`, size = 100), na.rm = TRUE)
}

# access from database ----------------------------------------------------
# for demonstration, using SQLite

# install.packages("RSQLite")
library(DBI)

SQLiteIsHere <- "05_02_associatedFiles/ucs_satellite.db" # is your working directory "Chapter 05" ?
mySQLiteDB <- dbConnect(RSQLite::SQLite(),SQLiteIsHere)

# add extended math functions
RSQLite::initExtension(mySQLiteDB)
randomMedianSQL <- function() {
  do_this_sqlite <- "
    SELECT `Launch Mass (kg.)`
    FROM satellites
    WHERE `Launch Mass (kg.)` != ''
    ORDER BY RANDOM()
    LIMIT 100
  "
  
  returnDF <- dbGetQuery(mySQLiteDB,do_this_sqlite)
  return(median(returnDF$`Launch Mass (kg.)`))
}

# Profiling ---------------------------------------------------------------

# profvis shows memory use.

# install.packages("profvis")
library(profvis)

profvis({
  randomMedianDF()
  randomMedianFile()
  randomMedianSQL()
})

# microbenchmark shows cpu use. 

# install.packages("microbenchmark")
library(microbenchmark)

benchmarkresults <- microbenchmark(
  DataFrame = randomMedianDF(),
  File = randomMedianFile(),
  SQL = randomMedianSQL()
)

boxplot(benchmarkresults)
