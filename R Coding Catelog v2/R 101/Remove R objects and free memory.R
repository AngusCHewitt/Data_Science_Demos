ls()  # list objects in global env
rm(a)  # delete object 'a'
rm(list = ls())  # caution: delete all objects in .GlobalEnv
gc()  # free system memory
