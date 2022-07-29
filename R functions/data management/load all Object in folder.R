##-0 fun to read in all objs ain a folder

read_In_objs <- function(obj_filepath, env) {

# store env = environent() as a global obj and pass in as a arg
#i.e. envir = environent() ~ read_In_objs("path", envir)  
##--simply place file path as arg and all obj will load into your env
path <- obj_filepath
files <- list.files(path = path, full.names = T) # store full names of all r objs
lapply(files, load, env) # load all obj to env

}