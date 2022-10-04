# Description: Use sparklyr with Rstudio

# install.packages("httpuv")
# install.packages("sparklyr")
library(sparklyr)

spark_install(version = "2.1.0")

# install.packages("devtools")
library(devtools)

devtools::install_github("rstudio/sparklyr")
# restart R (Session -> Restart R)

# connect to a local version of Spark
library(sparklyr)
sc <- spark_connect(master = "local")

# open the Spark UI. Connections tab, Spark UI, opens browser
spark_web(sc)
# load data into the spark cluster
library(dplyr)

ucsSat_tbl <- copy_to(sc, UCS_Satellite_Database, "spark_ucs")
# if you receive MalformedInputException, check encoding of source data
src_tbls(sc) # shows tables in spark

# use dplyr (tidyverse) to manipulate Spark
ucsSat_tbl %>% filter(Class_of_Orbit == "LEO")

# or use SQL
library(DBI)
dbGetQuery(sc, "SELECT `Class_of_ORBIT` FROM spark_ucs LIMIT 5")  

