library(cronR)

f <- system.file(package = "cronR", "extdata", "helloworld.R") # r file stored in cron package, run hello world and store data obs in home docs
cmd <- cron_rscript(f) # store r sciprt commad used by uni/linux terminal
cmd

cron_add(command = cmd, frequency = 'minutely', 
         id = 'test1', description = 'My process 1', tags = c('lab', 'xyz'))
cron_add(command = cmd, frequency = 'daily', at='7AM', id = 'test2')
cron_njobs() # schedule in jobs for specifiied time 

cron_ls() # view active cron jobs
cron_clear(ask=FALSE) # clear cron jobs
cron_ls()
