logistic_Mod_goodness_of_fit <- function(glm_Mod) {

## output qqnorm adjust of log distrn and res scatter plot with sim qquantile residuals 
require(statmod)
qres <- qresid(glm_Mod); qqnorm(qres, las=1); abline(0, 1)
scatter.smooth( qres~fitted(glm_Mod), las=1, main="Residuals vs fitted",
                xlab="Fitted value", ylab="Quantile residual")
}
