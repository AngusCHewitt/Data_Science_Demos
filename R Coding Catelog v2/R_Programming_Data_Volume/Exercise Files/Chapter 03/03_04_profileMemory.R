# Description: profile tools to identify memory issues

# r documentation at https://developer.r-project.org/memory-profiling.html

# profmem -----------------------------------------------------------------

install.packages("profmem")
library(profmem)

memoryResults <- profmem(UCS_Satellite_Database <- read.delim("../UCS_Satellite_Database", stringsAsFactors = FALSE))

head(memoryResults) # display operations and memory used

# This is memory allocated for operations - not memory finally used...
sum(memoryResults$bytes) # about 12368375 bytes
# ...which is approx 3x the size of the final object...
object.size(UCS_Satellite_Database) # about 3322752 bytes

# this is useful to tell you where/when memory is being used


# tracemem ----------------------------------------------------------------
# alert when objects are copied
# is this enabled in your build of R?
capabilities()["profmem"]

# demonstrate that nothing happens
UCS_Satellite_Database$newColumn <- runif(nrow(UCS_Satellite_Database))

tracemem(UCS_Satellite_Database) # turn on tracing

# now an alert is issued
UCS_Satellite_Database$newColumn <- runif(nrow(UCS_Satellite_Database))

untracemem(UCS_Satellite_Database) # turn off alert

tracingState() # is tracing on?
