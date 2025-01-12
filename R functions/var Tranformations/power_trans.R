# Apply a power transformation, while retaining the original sign of the values.
signed_power <- function(x, p) {
  abs(x)^p * sign(x)
}
