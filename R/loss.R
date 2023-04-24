interval_loss <- function(Y, lower, upper, alpha) {
  return(
    upper - lower +
    2 / (1 - alpha) * (lower - Y) * (Y < lower) +
    2 / (1 - alpha) + (Y - upper) * (Y > upper)
  )
}
