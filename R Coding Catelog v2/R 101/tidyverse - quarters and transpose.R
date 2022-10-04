# create quarterly tiem intervals (catergoisation)
x <- ymd(c("2012-03-26", "2012-05-04", "2012-09-23", "2012-12-31"))
quarter(x)
quarter(x, with_year = TRUE)
quarter(x, with_year = TRUE, fiscal_start = 11)
semester(x)
semester(x, with_year = TRUE)



