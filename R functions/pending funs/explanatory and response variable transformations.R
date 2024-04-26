##--- option for norm trans x and y vars 
##--purpuseofthe trans in to involve linear relation between x and y and remove bias outliers. 

library(forecast)
library(car)

# to find optimal lambda
##--  the option of local likelihood for positive values and the Julio for positive and negative values
lambda = BoxCox.lambda( data$qcc_centre_pos_sd )
# now to transform vector
trans.vector = BoxCox( data$qcc_centre_pos_sd, lambda)


##-- another boxcox trans options ftom car package
bcnPower(U, lambda, jacobian.adjusted = FALSE, gamma)


##--  form goalscoring fantasy QCC using box Cox transformation
Key_Forward_dt %>%
  ungroup() %>%
  mutate(goal_Scoring_fantasy_QCC = if_else(goal_Scoring_fantasy_QCC < 0, -log(abs(goal_Scoring_fantasy_QCC)+1),log(goal_Scoring_fantasy_QCC + 1))) -> Key_Forward_dt


# to find optimal lambda
lambda = BoxCox.lambda( Key_Forward_dt$goal_Scoring_fantasy_QCC)

##--add boxcox'stransform version of QCC fantasy starts
Key_Forward_dt %>%
ungroup ()%>%
mutate(qcc_centre_pos_sd_trans = BoxCox(Key_Forward_dt$qcc_centre_pos_sd, lambda)) -> Key_Forward_dt

## note;  If your transformation involves +, *, ^, or -, you’ll need to wrap it in I() so R doesn’t treat it like part of the model specification

##-- That means you can use a polynomial function to get arbitrarily close to a smooth function by fitting an equation like y = a_1 + a_2 * x + a_3 * x^2

##-- poly transformation to capture NL relation x ~ y
model_matrix(df, y ~ poly(x, 2))
#> # A tibble: 3 x 3
#>   `(Intercept)` `poly(x, 2)1` `poly(x, 2)2`
#>           <dbl>         <dbl>         <dbl>
#> 1             1     -7.07e- 1         0.408
#> 2             1     -7.85e-17        -0.816
#> 3             1      7.07e- 1         0.408

##-- natural spline more stable fun than one above, same type NL transformation
library(splines)
model_matrix(df, y ~ ns(x, 2))
#> # A tibble: 3 x 3
#>   `(Intercept)` `ns(x, 2)1` `ns(x, 2)2`
#>           <dbl>       <dbl>       <dbl>
#> 1             1       0           0
#> 2             1       0.566      -0.211
#> 3             1       0.344       0.771


##-- Rcompanina
### Log-normal distribution example
Conc = rlnorm(100)
Conc.trans = transformTukey(Conc)


##--sin and cos , asin and acos provide metrical and semi symmetrical distributions i.e. sin values 1 and -1

boxplot(sin(upper_Range_cum_lagged_Goal_scoring_Fatasy_scores - Med_cum_lagged_Contested.Possessions_minus_CM))

boxplot(asin(upper_Range_cum_lagged_Goal_scoring_Fatasy_scores - Med_cum_lagged_Contested.Possessions_minus_CM))
