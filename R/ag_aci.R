#'
initialize_ag_aci <- function(object) {
  default_parameters <- list(
    gamma = 0.01,
    interval_constructor = "conformal",
    conformity_score = "absolute_error",
    gamma_grid = c(0.001, 0.002, 0.004, 0.008, 0.016, 0.032, 0.064, 0.128),
    symmetric = TRUE,
    conditional = FALSE,
    base_method = "ACI"
  )

  acceptable_parameters <- list(
    interval_constructor = c("conformal", "linear", "recenter"),
    conformity_score = c("absolute_error"),
    asymmetric = c(FALSE, TRUE),
    conditional = c(FALSE, TRUE),
    base_method = c("ACI", "recenter")
  )

  if(is.null(object$internal)) {
    object$parameters <- initialize_parameters(
      object$parameters,
      default_parameters,
      acceptable_parameters
    )

    if(object$parameters$symmetric == TRUE) {
      ntheta <- ifelse(object$parameters$conditional, ncol(object$X), 1)
    }
    else {
      ntheta <- ifelse(object$parameters$conditional, ncol(object$X) * 2, 2)
    }

    if(!is.null(object$parameters$theta0)) {
      theta0 <- rep(object$parameters$theta0, ntheta)
    }

    if(tolower(object$parameters$interval_constructor) == "conformal") {
      interval_constructor <- interval_constructor_conformity(object$parameters$conformity_score, object$parameters$symmetric)
      if(is.null(object$parameters$theta0))
        theta0 <- rep(object$alpha, ntheta)
    }
    else if(tolower(object$parameters$interval_constructor) == "linear") {
      interval_constructor <- interval_constructor_linear(object$parameters$symmetric)
      if(is.null(object$parameters$theta0))
        theta0 <- rep(0, ntheta)
    }
    else if(tolower(object$parameters$interval_constructor) == "recenter") {
      interval_constructor <- interval_constructor_recenter()
      ntheta <- 2
      if(is.null(object$parameters$theta0))
        theta0 <- rep(0, ntheta)
    }

    # Initialize set of candidate learners
    candidate_acis <- lapply(object$parameters$gamma_grid, function(gamma) {
      aci(
        X = object$X,
        alpha = object$alpha,
        method = object$parameters$base_method,
        parameters = list(
          gamma = gamma,
          theta0 = theta0,
          interval_constructor = object$parameters$interval_constructor,
          conformity_score = object$parameters$conformity_score,
          conditional = object$parameters$conditional,
          symmetric = object$parameters$symmetric
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
#' @importFrom stats predict
update_ag_aci <- function(object, Y, predictions, X = NULL, training = FALSE) {
  n <- length(Y)

  if(training == TRUE) {
    object$Y           <- c(object$Y, Y)
    object$predictions <- rbind(object$predictions, predictions)
    object$covered     <- c(object$covered, rep(NA, length(Y)))
    object$intervals   <- rbind(object$intervals, matrix(rep(NA, 2 * length(Y)), ncol = 2, nrow = length(Y)))

    # Update each of the candidate ACIs
    object$internal$candidate_acis <- lapply(object$internal$candidate_acis, update.aci, newY = Y, newpredictions = predictions, newX = X, training = training)
  }
  else {
    # Update each of the candidate ACIs
    object$internal$candidate_acis <- lapply(object$internal$candidate_acis, update.aci, newY = Y, newpredictions = predictions, newX = X, training = training)

    lower_candidates <- matrix(unlist(lapply(object$internal$candidate_acis, function(aci) tail(aci$intervals[, 1], n))), nrow = n, byrow = FALSE)
    upper_candidates <- matrix(unlist(lapply(object$internal$candidate_acis, function(aci) tail(aci$intervals[, 2], n))), nrow = n, byrow = FALSE)

    colnames(lower_candidates) <- object$parameters$gamma_grid
    colnames(upper_candidates) <- object$parameters$gamma_grid

    # Update the expert aggregation methods
    object$internal$experts$lower <- predict(object$internal$experts$lower, newexperts = lower_candidates, newY = Y, type = "model")
    object$internal$experts$upper <- predict(object$internal$experts$upper, newexperts = upper_candidates, newY = Y, type = "model")

    intervals <- matrix(c(
      tail(object$internal$experts$lower$prediction, length(Y)),
      tail(object$internal$experts$upper$prediction, length(Y))
    ), ncol = 2, byrow = FALSE)

    covered <- Y >= intervals[,1] & Y <= intervals[,2]

    object$intervals   <- rbind(object$intervals, intervals)
    object$covered     <- c(object$covered, covered)
    object$Y           <- c(object$Y, Y)
    object$predictions <- base::rbind(object$predictions, predictions)

    if(!is.null(X)) {
      object$X <- rbind(object$X, X)
    }
  }

  return(object)
}

predict_ag_aci <- function(object, prediction, X = NULL) {
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
