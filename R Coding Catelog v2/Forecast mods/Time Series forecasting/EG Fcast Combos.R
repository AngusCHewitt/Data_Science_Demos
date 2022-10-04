# training and test d.s. are a colloection of matrices 
obs <- rnorm(100)
preds <- matrix(rnorm(1000, 1), 100, 10)
train_o <- obs[1:80]
train_p <- preds[1:80,]
test_o <- obs[81:100]
test_p <- preds[81:100,]

# forecast and acutals d.s.
data <- foreccomb(train_o, train_p, test_o, test_p)

is(preds)

# find the optimial fcast combination, min RMSE 
best_combination <- auto_combine(data, criterion = "RMSE")
summary(best_combination)

sum(best_combination$Weights)
