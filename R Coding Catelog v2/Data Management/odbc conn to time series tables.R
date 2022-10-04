library(odbc)
library(RODBC)

con <- odbcConnect( "TimeSeries")
id_block <- sqlQuery(con,"SELECT * FROM idBlock")
odbcClose(con) # end connection - good prac. run at end session

tables <- as.data.frame(sqlTables(con)) # view all VEMD tables names - details
View(tables)

Table_col <- as.data.frame(sqlColumns(con , "RENAL_DIALYSIS_RPT")) # col names & var character. for table
View(Table_col)

REG_DATA <- sqlFetch(con, "REGISTRY_DATA") # create table from sql table

RODBC::
  
  search()
