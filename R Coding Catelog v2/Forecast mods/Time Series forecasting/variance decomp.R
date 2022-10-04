# quantify variance explained by each var.

data("mtcars")

cars_lm <- lm(mpg ~. , data = mtcars)

plot(cars_lm)

summary(cars_lm)

# display sum sqs for each variablr in an anova table
anova_stats <- anova(cars_lm)

# d.f. with decomp variance stats 
sumsq <- data.frame(vars = row.names(anova_stats), variance_exp = anova_stats$`Sum Sq`/ sum(anova_stats$`Sum Sq`)*100)

cars_nlm <- nlm(mpg ~. , data = mtcars)

# non linear model i.e. poission uses residual deviance in anova table
## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
print(d.AD <- data.frame(treatment, outcome, counts))
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
anova(glm.D93)
summary(glm.D93)