# Description: Sample Dataset for R Programming in Data Science: High Volume Data 

# https://www.ucsusa.org/nuclear-weapons/space-weapons/satellite-database

  # import
  UCS_Satellite_Database <-
    read.delim("../UCS_Satellite_Database", 
               stringsAsFactors = FALSE,
               fileEncoding = "latin1")
  
  # clean
  UCS_Satellite_Database$Launch.Mass..kg.. <-
    as.numeric(gsub(",", "", UCS_Satellite_Database$Launch.Mass..kg..))
  UCS_Satellite_Database$Power..watts. <-
    as.numeric(gsub(",", "", UCS_Satellite_Database$Power..watts.))
  UCS_Satellite_Database$Apogee..km. <-
    as.numeric(gsub(",", "", UCS_Satellite_Database$Apogee..km.))
  UCS_Satellite_Database$Perigee..km. <-
    as.numeric(gsub(",", "", UCS_Satellite_Database$Perigee..km.))
  UCS_Satellite_Database$Class.of.Orbit <-
    as.factor(trimws(UCS_Satellite_Database$Class.of.Orbit))

# look at the file
nrow(UCS_Satellite_Database)
head(UCS_Satellite_Database)
