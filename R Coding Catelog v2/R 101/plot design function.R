 
 # datasets used in R modelling
 library(HSAUR)

 # plot means for Rats weight by mother and litter genome type
 ?plot.design()

 plot.design(foster)
 plot.design(foster,fun=median)
 plot.design(foster,fun=sd)