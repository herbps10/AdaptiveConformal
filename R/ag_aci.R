#'
initialize_ag_aci <- function(object) {
  if(is.null(object$internal)) {
    if(is.null(object$parameters$gamma_grid)) {
      object$parameters$gamma_grid <- c(0.001, 0.002, 0.004, 0.008, 0.016, 0.032, 0.065, 0.128)
    }

    theta0 <- ifelse(object$parameters$interval_constructor == "conformal", object$alpha, 0)

    # Initialize set of candidate learners
    candidate_acis <- lapply(object$parameters$gamma_grid, function(gamma) {
      aci(
        alpha = object$alpha,
        method = "RollingRC",
        parameters = list(
          gamma = gamma,
          theta0 = object$alpha,
          interval_constructor = object$parameters$interval_constructor,
          conformity_score = object$parameters$conformity_score
        )
      )
    })

    # Initialize expert aggregation
    experts <- list(
      lower = opera::mixture(model = "BOA", loss.type = list(name = "pinball", tau = (1 - object$alpha) / 2)),
      upper = opera::mixture(model = "BOA", loss.type = list(name = "pinball", tau = 1 - (1 - object$alpha) / 2))
    )

    object$internal <- list(
      gamma_grid = object$parameters$gamma_grid,
      candidate_acis = candidate_acis,
      experts = experts
    )
  }

  return(object)
}

#' @importFrom utils tail
update_ag_aci <- function(object, Y, predictions, training = FALSE) {
  n <- length(Y)
  prediction_matrix <- is.matrix(predictions)

  if(training == TRUE) {
    object$Y <- c(object$Y, Y)
    if(is.matrix(predictions)) {
      object$predictions <- rbind(object$predictions, predictions)
    }
    else {
      object$predictions <- rbind(object$predictions, t(t(predictions)))
    }
    object$covered  <- c(object$covered, rep(NA, length(Y)))
    object$intervals <- rbind(object$intervals, matrix(rep(NA, 2 * length(Y)), ncol = 2, nrow = length(Y)))

    # Update each of the candidate ACIs
    object$internal$candidate_acis <- lapply(object$internal$candidate_acis, update.aci, newY = Y, newpredictions = predictions, training = training)
  }
  else {
    # Update each of the candidate ACIs
    object$internal$candidate_acis <- lapply(object$internal$candidate_acis, update.aci, newY = Y, newpredictions = predictions, training = training)

    lower_candidates <- matrix(unlist(lapply(object$internal$candidate_acis, function(aci) tail(aci$intervals[, 1], n))), nrow = n, byrow = FALSE)
    upper_candidates <- matrix(unlist(lapply(object$internal$candidate_acis, function(aci) tail(aci$intervals[, 2], n))), nrow = n, byrow = FALSE)

    # Update the expert aggregation methods
    object$internal$experts$lower <- predict.aci(object$internal$experts$lower, newexperts = lower_candidates, newY = Y, type = "model")
    object$internal$experts$upper <- predict.aci(object$internal$experts$upper, newexperts = upper_candidates, newY = Y, type = "model")

    intervals <- matrix(c(
      tail(object$internal$experts$lower$prediction, length(Y)),
      tail(object$internal$experts$upper$prediction, length(Y))
    ), ncol = 2, byrow = FALSE)

    covered <- Y >= intervals[,1] & Y <= intervals[,2]

    object$intervals   <- rbind(object$intervals, intervals)
    object$covered     <- c(object$covered, covered)
    object$Y           <- c(object$Y, Y)
    object$predictions <- base::rbind(object$predictions, predictions)
  }

  return(object)
}

predict_ag_aci <- function(object, prediction) {
  # Collect the predictions of each of the candidate ACIs
  candidate_intervals <- matrix(
    unlist(lapply(object$internal$candidate_acis, predict.aci, prediction)),
    ncol = 2, byrow = TRUE
  )

  lower_weights  <- object$internal$experts$lower$coefficients
  upper_weights  <- object$internal$experts$upper$coefficients

  if(is.null(lower_weights)) lower_weights <- rep(1 / nrow(candidate_intervals), nrow(candidate_intervals))
  if(is.null(upper_weights)) upper_weights <- rep(1 / nrow(candidate_intervals), nrow(candidate_intervals))

  lower <- candidate_intervals[, 1] %*% lower_weights
  upper <- candidate_intervals[, 2] %*% upper_weights

  return(c(lower, upper))
}
