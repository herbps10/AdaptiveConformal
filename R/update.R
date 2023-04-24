#' Update an ACI object with a new observation
#'
#' @param object object of class "aci"
#' @param newY new observation
#' @param newpredictions new predictions
#'
#' @export
update.aci <- function(object, newY, newpredictions, training = FALSE) {
  method <- match.arg(object$method, c("ACI", "AgACI", "FACI"))

  n <- length(newY)
  prediction_matrix <- is.matrix(newpredictions)
  if(prediction_matrix && n != nrow(newpredictions)) {
    stop("Length of newY and number of rows in newpredictions must be the same.")
  }
  else if(!prediction_matrix && n != length(newpredictions)) {
    stop("Length of newY and length of newpredictions must be the same.")
  }

  updaters <- list(
    ACI   = update_aci,
    AgACI = update_ag_aci,
    FACI  = update_faci
  )

  object <- updaters[[method]](object, Y = newY, predictions = newpredictions, training = training)

  object$training <- c(object$training, rep(training, length(newY)))
  object$coverage <- mean(object$covered[object$training == FALSE])

  object
}
