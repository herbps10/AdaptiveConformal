#' Update an ACI object with a new observation
#'
#' @param object object of class "aci"
#' @param newY new observation
#' @param newpredictions new predictions
#' @param training boolean indicating whether to use new observations and predictions solely as training (TRUE)
#' or to also update the ACI method (TRUE)
#' @param ... additional arguments (currently)
#'
#' @examples
#' # Generate a simple time series of observations
#' N <- 1e3
#' Y <- rnorm(N)
#' predictions <- rep(0, N) # predict 0 at very timestep
#'
#' # Initialize Rolling Risk Control algorithm
#' result <- aci(method = "RollingRC", parameters = list(gamma = 0.01))
#'
#' # Observe outcomes one at a time and update ACI method
#' for(t in 1:N) {
#'   result <- update(result, newY = Y[t], newpredictions = predictions[t])
#' }
#'
#' summary(result)
#'
#' # Alternatively, multiple observations and predictions can be supplied to
#' # the update function at the same time
#' result <- aci(method = "RollingRC", parameters = list(gamma = 0.01))
#'
#' # Observe outcomes one at a time and update ACI method
#' result <- update(result, newY = Y, newpredictions = predictions)
#'
#' summary(result)
#'
#' # We may have some initial observations and predictions we would like to use
#' # as context (e.g. for calculating conformity scores) but we do not want to use
#' # in the online learning algorithm for ACI.
#' # For example, suppose that we want to use the first 500 observations as context,
#' # and only generate intervals for the second 500 observations.
#' result <- aci(method = "RollingRC", parameters = list(gamma = 0.01))
#'
#' result <- update(result, newY = Y[1:500], newpredictions = predictions[1:500], training = TRUE)
#' result <- update(result, newY = Y[501:1000], newpredictions = predictions[501:1000])
#'
#' summary(result)
#'
#' @export
update.aci <- function(object, newY, newpredictions, training = FALSE, ...) {
  method <- match.arg(object$method, c("RollingRC", "AgACI", "FACI"))

  n <- length(newY)
  prediction_matrix <- is.matrix(newpredictions)
  if(prediction_matrix && n != nrow(newpredictions)) {
    stop("Length of newY and number of rows in newpredictions must be the same.")
  }
  else if(!prediction_matrix && n != length(newpredictions)) {
    stop("Length of newY and length of newpredictions must be the same.")
  }

  updaters <- list(
    RollingRC  = update_rolling_rc,
    AgACI = update_ag_aci,
    FACI  = update_faci
  )

  object <- updaters[[method]](object, Y = newY, predictions = newpredictions, training = training)

  object$training <- c(object$training, rep(training, length(newY)))

  # Calculate metrics
  observed <- object$training == FALSE
  object$coverage <- mean(object$covered[observed])
  object$mean_width <- mean(object$intervals[observed, 2] - object$intervals[observed, 1])
  object$mean_interval_loss <- mean(interval_loss(object$Y[observed], object$intervals[observed, 1], object$intervals[observed, 2], object$alpha))

  object
}
