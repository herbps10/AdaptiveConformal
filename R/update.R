#' Update an ACI object with a new observation
#'
#' @param object object of class "aci"
#' @param newY new observation
#' @param newpredictions new predictions
#' @param newX new covariates
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
#' # Initialize ACI algorithm
#' result <- aci(method = "ACI", parameters = list(gamma = 0.01))
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
#' result <- aci(method = "ACI", parameters = list(gamma = 0.01))
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
#' result <- aci(method = "ACI", parameters = list(gamma = 0.01))
#'
#' result <- update(result, newY = Y[1:500], newpredictions = predictions[1:500], training = TRUE)
#' result <- update(result, newY = Y[501:1000], newpredictions = predictions[501:1000])
#'
#' summary(result)
#'
#' @export
update.aci <- function(object, newY, newpredictions, newX = NULL, training = FALSE, ...) {
  method <- match.arg(object$method, aci_methods())

  n <- length(newY)
  prediction_matrix <- is.matrix(newpredictions)
  if(prediction_matrix && n != nrow(newpredictions)) {
    stop("Length of newY and number of rows in newpredictions must be the same.")
  }
  else if(!prediction_matrix && n != length(newpredictions)) {
    stop("Length of newY and length of newpredictions must be the same.")
  }

  updaters <- list(
    SCP = update_scp,
    ACI = update_aci,
    AgACI = update_ag_aci,
    DtACI = update_dtaci,
    "SF-OGD" = update_sfogd,
    SAOCP = update_saocp
  )

  if(!is.null(newX) && is.vector(newX)) {
    newX <- matrix(newX, ncol = length(newX))
  }

  if(!is.null(newX)) {
    if(nrow(newX) != n) {
      stop("Length of newY and number of rows in newX must be the same.")
    }
  }

  if(is.vector(newpredictions)) {
    newpredictions <- matrix(newpredictions, ncol = 1)
  }

  object <- updaters[[method]](object, Y = newY, predictions = newpredictions, X = newX, training = training)

  object$intervals <- unname(object$intervals)

  object$training <- c(object$training, rep(training, length(newY)))

  # Calculate metrics
  object$metrics <- aci_metrics(object)

  object
}

pinball_loss <- function(alpha, theta, r) {
  ifelse(theta >= r, (1 - alpha) * (theta - r), alpha * (r - theta))
}

#' Compute metrics for ACI prediction intervals.
#'
#' @param object ACI object
#' @param indices indices of observations to include in computation of metrics
#' @param interval_width interval width used for calculating strongly adaptive regret
#' @importFrom stats sd
#' @importFrom slider slide_dbl
#' @export
aci_metrics <- function(object, indices = NULL, interval_width = 20) {
  if(is.null(indices)) {
    indices <- 1:length(object$Y)
  }

  observed <- object$training == FALSE

  indices <- (1:length(object$Y) %in% indices) & observed

  coverage <- mean(object$Y[indices] >= object$intervals[indices, 1] & object$Y[indices] <= object$intervals[indices, 2] & object$intervals[indices, 1] <= object$intervals[indices,2])
  diff <- object$intervals[indices, 2] - object$intervals[indices, 1]
  mean_width <- mean(ifelse(diff < 0, 0, diff))
  sd_width   <- sd(ifelse(diff < 0, 0, diff))
  mean_interval_loss <- mean(interval_loss(object$Y[indices], object$intervals[indices, 1], object$intervals[indices, 2], object$alpha))
  below <- mean(object$Y[indices] < object$intervals[indices, 1])
  above <- mean(object$Y[indices] > object$intervals[indices, 2])
  path_length <- sum(abs(diff(object$intervals[indices, 2] - object$intervals[indices, 1])))

  residuals <- object$Y[indices] - object$predictions[indices,]
  losses <- slide_dbl(pinball_loss(object$alpha, residuals, diff / 2), mean, .after = interval_width, .step = 1)
  optimal_radii <- slide_dbl(residuals, function(x) quantile(x, object$alpha), .after = interval_width, .step = 1)
  optimal_losses <- slide_dbl(seq_along(residuals), function(i) mean(pinball_loss(object$alpha, residuals[i], optimal_radii[i]), .after = interval_width, .step = 1))
  strongly_adaptive_regret <- max(losses - optimal_losses)

  conditional_coverage <- NA
  if(!is.null(object$X) && nrow(object$X) == length(indices)) {
    conditional_coverage <- (object$covered[indices] %*% object$X[indices, ]) / colSums(object$X[indices,,drop = FALSE])
  }

  return(list(
    coverage = coverage,
    mean_width = mean_width,
    sd_width = sd_width,
    mean_interval_loss = mean_interval_loss,
    below = below,
    above = above,
    conditional_coverage = conditional_coverage,
    interval_width = interval_width,
    strongly_adaptive_regret = strongly_adaptive_regret,
    path_length = path_length
  ))
}
