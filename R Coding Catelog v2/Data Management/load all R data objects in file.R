
##-- store file path with all mod var cof dts 
path <- "Data/Mod coef combos/"
files <- list.files(path=path, full.names = T)

##-- load all datatables to the workspace
lapply(files, load, environment())