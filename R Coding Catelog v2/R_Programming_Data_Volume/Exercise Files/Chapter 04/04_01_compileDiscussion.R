
install.packages("Rcpp")
install.packages("Rtools")
library(Rcpp)

# Rcpp example ------------------------------------------------------------

cppFunction('int add(int x, int y) {
            int sum = x + y;
            return sum;
            }')

add(1,2)
