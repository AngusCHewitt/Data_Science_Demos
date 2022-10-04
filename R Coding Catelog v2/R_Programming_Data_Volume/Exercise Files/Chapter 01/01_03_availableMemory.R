# Description: How much memory is used?
  
memoryResults <- memory.profile()

object.size(UCS_Satellite_Database) # ~ 3 MB
newobject <- UCS_Satellite_Database[ , 1:10]
object.size(newobject) # Size INCREASED?!?

# garbage collection & memory report
# generates a memory report. garbage collection is automatic
gc()
