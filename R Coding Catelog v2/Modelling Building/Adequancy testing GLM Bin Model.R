years <- c(5.8,15.0,21.5,27.5,33.5,39.5,46.0,51.5)

cases <- c(0,1,3,8,9,8,10,5)

miners <- c(98,54,43,48,51,38,28,11)

ymat <- cbind(years,cases,miners)
ymat <- cbind(years,miners-cases)
ashford <- data.frame(ymat,years)
ymat
anal <- glm(ymat ~ years,fmaily=binomial,data=ashford)

anal <- glm(ymat ~ years,family=binomial,data=ashford)

Number of Fisher Scoring iterations: 4

pred_prob <- anal$fit
eta_hat <- anal$linear.predictor
dev_res <- residuals(anal,c="deviance")

influence.measures(anal)

df <- dfbetas(anal)
df_int <- df[,1]
df_years <- df[,2]

hat <- hatvalues(anal)
qqnorm(dev_res)
plot(pred_prob,dev_res)
plot(eta_hat,dev_res)
plot(years,dev_res)
plot(hat,dev_res)
plot(pred_prob,df_years)
plot(hat.df,df_years)



