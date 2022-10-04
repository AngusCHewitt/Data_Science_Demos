# patient pathways in June / July 2012

# illustrates patient flows from NHD to NHND ;

library(msm)
library(Rcmdr)

Dataset <- readXL("//internal.vic.gov.au/DHHS/HomeDirs6/ahew2812/Documents/Renal Register 2012-06-07.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="Registry Data", 
         stringsAsFactors=TRUE)

str(Dataset)


statetable.msm(Stage, PatientCode, data=Dataset)

#to
#from    1    2    3    4    5    6    7
#1  251    3    0    0   15    0   20
#2   10  149    0    0    7    1   19
#3    1    0   71    1    0    2    5
#4    3    3    3    3    2    3    9
#5    3    4    3    4  200    2   51
#6    0    0    0  123    0    0    2
#7   41   28    8    1   45   17 1859