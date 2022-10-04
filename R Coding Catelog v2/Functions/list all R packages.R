# list all available packages for your version of R
av <- names(available.packages()[,1])

# return count of all available packages
length(av)
