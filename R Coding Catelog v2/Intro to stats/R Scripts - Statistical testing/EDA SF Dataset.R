## read excel file SF Dataset 
Dataset <- read.table("H:/My Documents/SF Dataset.csv", header=TRUE, 
                      sep=",", na.strings="NA", dec=".", strip.white=TRUE)


str(Dataset)

## coerce numeric vari in factors
Dataset <- within(Dataset, {
  At.risk.prior <- as.factor(At.risk.prior)
  CLIB.prior <- as.factor(CLIB.prior)
  Gap...12mths. <- as.factor(Gap...12mths.)
  Interpreter <- as.factor(Interpreter)
  MH.prior <- as.factor(MH.prior)
  Opioids.0.3.prior <- as.factor(Opioids.0.3.prior)
  Opioids.12.15.post <- as.factor(Opioids.12.15.post)
  Opioids.24.27.post <- as.factor(Opioids.24.27.post)
  Term.dev.0.3.prior <- as.factor(Term.dev.0.3.prior)
  Term.dev.12.15.post <- as.factor(Term.dev.12.15.post)
  Term.dev.24.27.post <- as.factor(Term.dev.24.27.post)
})

## Create a glm for post opioid usage
GLM.7 <- glm(formula = Opioids.12.15.post ~ Opioids.0.3.prior + CLIB.prior + 
      Gap...12mths. + Interpreter + MH.prior + treatment + Capacity.month.prior.to.surgery, 
    family = binomial(logit), data = Dataset)

# odds of treatment group taking  status = opioids vs control group

confidence <- Confint(GLM.7, level=0.95, type="LR")

str(confidence)