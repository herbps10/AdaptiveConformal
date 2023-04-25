#' Absolute error conformity score: S_t := |Y_t - \eqn{\hat{Y}}{prediction}_t|
#'
#' @param Y observed value
#' @param prediction predicted value
#'
conformity_score_absolute_error <- function(Y, prediction) {
  abs(Y - prediction)
}
