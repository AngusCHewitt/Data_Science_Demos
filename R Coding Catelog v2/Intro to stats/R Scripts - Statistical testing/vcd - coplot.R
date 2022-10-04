# using the searchoath to examine funcstions within the vcd package
?vcd::cotab_mosaic(~ Admit + Gender | Dept, data = UCBAdmissions)


sf_data <- read.csv("SF.csv")

# mosaic split into panels = treatment
sf <- cotab_mosaic(sf_data, condvars = "treatment",shade=TRUE,labeling=labeling_values)
cotabplot(~ Opioids.prior + CLIB.prior | treatment, 
          data = sf_data, 
          panel = sf)

# create a xtab table object for the below plot
Table <- 
  xtabs(~treatment+Capacity.month.prior.to.surgery+CLIB.prior+Gap_12mths, 
        data=sf_data)

#  mosaic matrix using the pairs function
pairs(Table,shade=TRUE)


