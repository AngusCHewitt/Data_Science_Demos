# Testing Growth Decomp Function

Dataset2 <- read.csv("testing function dataset.csv")

attach(Dataset2)

# reorder factor levels for age
Dataset$Age <- with(Dataset2, factor(Age, levels=c(' 0–4',' 5–9',' 10–14',
' 15–19',' 20–24',' 25–29',' 30–34',' 35–39',' 40–44',' 45–49',' 50–54',
' 55–59',' 60–64',' 65–69',' 70–74',' 75–79',' 80–84',' 85+')))

# create tables for both population populations

  table_pop1 <- tapply(Pop1 , list(Factor1,Factor2), sum, na.rm=TRUE)
  
  tot_pop1_factor1 <- tapply(Pop1  , list(Factor1), sum, na.rm=TRUE)

  table_pop2 <- tapply(Pop2, list(Factor1,Factor2), sum, na.rm=TRUE)
  
  tot_pop2_factor1 <- tapply(Pop2, list(Factor1), sum, na.rm=TRUE)


# table for Rates

  table_rate1  <- tapply(Rate1 , list(Factor1,Factor2), sum, na.rm=TRUE)
  table_rate2  <- tapply(Rate2 , list(Factor1,Factor2), sum, na.rm=TRUE)

# table sample sizes

  sample_1 <- tapply(N1, list(Factor1,Factor2), sum, na.rm=TRUE)
  sample_2 <- tapply(N2, list(Factor1,Factor2), sum, na.rm=TRUE)

# table totals Rate2 and N2
   
  total_sample2 <- (tapply(N2, list(Factor1), sum, na.rm=TRUE))
  total_rate2 <- (total_sample2  /  tot_pop2_factor1 ) *  1000 


#-- Component A: Hold constant Factors 1 & 2 structure and allow  
# pop growth and presentation rates to vary --#

# sum pop1 sample size(N1) 
  
  sum_N1 <-  sum(table_pop1 * table_rate1 /1000)


# EXPECTED ESTIMATED RESIDENT POPULATION IF 2005 AGE-SEX 
## STRUCTURE WAS HELD CONSTANT

# Pop1 distribution across both factors  

  pop1_distrn <- table_pop1/sum(table_pop1)

# Multiply Pop1 Distrn with Pop2 total

  multiply_distrn <- pop1_distrn * sum(table_pop2)


# Rate2 * Standardised Pop2

  standardised_pop1 <- sum(multiply_distrn  * Rates2010/1000)

# effect difference from vary pop

  effect_diff_A <- standardised_pop1 - sum_N1

#-- Component B: Change attributable to Population, 
## stardardise Sample (N) & Rates --#

standardise_N_rates <- sum(standardised_pop1 * Rate1/1000)

# effect difference produced from varying population
  
effect_diff_B <- standardise_N_rates - sum_N1
