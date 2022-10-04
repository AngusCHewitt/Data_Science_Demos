library(nonpar)

## Eg's of cochrane Q and stuart maxwell tests 
test <- matrix(c(1,1,1,1,1,1,
              1,1,0,1,1,1,
              0,0,0,1,0,0,
              0,1,0,0,1,1), nrow=6, ncol=4)

# n * p row and col dimesions
cochrans.q(test)

test2 <- matrix(c(12, 30, 13, 7, 70, 34, 3, 20, 32), nrow=3,ncol=3)

# 3 * 3 row and col dimesions
stuart.maxwell(test2)


Dataset <- read.table("H:/My Documents/SF Opioids.csv", header=TRUE, 
  sep=",", na.strings="NA", dec=".", strip.white=TRUE)

# coerce variables into factors
Dataset <- within(Dataset, {
  At_risk_prior <- as.factor(At_risk_prior)
  Capacity_month_prior <- as.factor(Capacity_month_prior)
  CLIB_prior <- as.factor(CLIB_prior)
  Gap_12mths <- as.factor(Gap_12mths)
  Interpreter <- as.factor(Interpreter)
  MH_0.3.prior <- as.factor(MH_0.3.prior)
  Opioids_0.3_prior_Binary <- as.factor(Opioids_0.3_prior_Binary)
  Opioids_12.15 <- as.factor(Opioids_12.15)
  Opioids_12.15_post_Binary <- as.factor(Opioids_12.15_post_Binary)
  Opioids_24.26 <- as.factor(Opioids_24.26)
  Opioids_24.27_post_Binary <- as.factor(Opioids_24.27_post_Binary)
  Opioids_prior <- as.factor(Opioids_prior)
})
library(abind, pos=17)

#run a stuart maxwell test on per and post opioids 12-15 months
  Table <- xtabs(~Opioids_prior+Opioids_12.15, data=Dataset)
  cat("\nFrequency table:\n")
  print(Table)
  Test <- stuart.maxwell(Table)
  print(Test)

# bar chart of SF group opioid pre and post surg 12-15 months
with(Dataset, Barplot(Opioids_12.15, by=Opioids_prior, style="divided", 
legend.pos="above", xlab="Opioids_12.15", ylab="Frequency"))

# bar chart of SF group opioid pre (% of column total) and post surg 12-15 months
with(Dataset, Barplot(Opioids_12.15, by=Opioids_prior, style="divided", 
legend.pos="above", xlab="Opioids_12.15", ylab="Percent", scale="percent"))

# mosaic plot of opioid prior and post 12-15 Months
mosaic(~Opioids_prior+Opioids_12.15, data=Dataset,shade=FALSE,
main = " mosaic plot of opioid prior and post 12-15 Months",
labeling = labeling_values,direction = "v", rot_labels=c(45,0,0,45))


#run a stuart maxwell test on per and post opioids 12-15 months
  Table <- xtabs(~Opioids_prior+Opioids_24.26, data=Dataset)
  cat("\nFrequency table:\n")
  print(Table)
  Test <- stuart.maxwell(Table)
  print(Test)

# bar chart of SF group opioid pre and post surg 24-26 months
with(Dataset, Barplot(Opioids_24.26, by=Opioids_prior, style="divided", 
legend.pos="above", xlab="Opioids 24 to 26 Months", ylab="Frequency"))

# bar chart of SF group opioid pre (% of column total) and post surg 12-15 months
with(Dataset, Barplot(Opioids_12.15, by=Opioids_prior, style="divided", 
legend.pos="above", xlab="Opioids 24 to 26 Months", ylab="Percent", scale="percent"))


