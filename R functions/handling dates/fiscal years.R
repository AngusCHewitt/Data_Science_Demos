#  AUS fiscal years

fiscal_Year_fun <- function(date_Vec) {
  
  require(busdater) # business dates
  require(tidyverse) # tidy data
  
  # dates  = date vector you wish to convert to fiscal years
  Fiscal_Year <- str_c(get_fy(date_Vec, offset_period = -1),"/",get_fy(date_Vec)) # Aus fiscal
}

