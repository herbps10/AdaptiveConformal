# Initialize a DtACI object
initialize_dtaci <- function(object) {
  default_parameters <- list(
    I = 100,
    interval_constructor = "conformal",
    conformity_score = "absolute_error",
    gamma_grid = c(0.001, 0.002, 0.004, 0.008, 0.016, 0.032, 0.064, 0.128),
    conditional = FALSE,
    symmetric = TRUE,
    base_method = "ACI",
    eta = NULL
  )

  acceptable_parameters <- list(
    interval_constructor = c("conformal", "linear"),
    conformity_score = c("absolute_error"),
    conditional = c(FALSE),
    symmetric = c(TRUE)
  )

  if(is.null(object$internal)) {
    object$parameters <- initialize_parameters(
      object$parameters,
      default_parameters,
      acceptable_parameters
    )

    if(object$parameters$interval_constructor == "conformal") {
      interval_constructor <- interval_constructor_conformity("absolute_error")
    }
    else if(object$parameters$interval_constructor == "linear") {
      interval_constructor <- interval_constructor_linear(object$parameters$symmetric)
    }

    # K is the number of candidate gamma values
    K = length(object$parameters$gamma_grid)

    if(is.null(object$parameters$eta)) {
      alpha <- object$alpha
      object$parameters$eta <- sqrt(3 / object$parameters$I) * sqrt((log(K * object$parameters$I) + 2) / ((alpha)^2 * (1 - alpha)^3 + (1 - alpha)^2 * (alpha)^3))
    }

    if(is.null(object$parameters$sigma)) {
      object$parameters$sigma <- 1 / (2 * object$parameters$I)
    }

    theta0 <- object$parameters$theta0

    if(is.null(theta0)) {
      if(object$parameters$interval_constructor == "linear") theta0 <- 0
      if(object$parameters$interval_constructor == "conformal") theta0 <- object$alpha
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

    object$internal <- list(
      candidate_acis = candidate_acis,
      K = K,
      theta = matrix(nrow = 0, ncol = 1),
      losses = matrix(nrow = 0, ncol = 1),
      weights = matrix(rep(1/K, K), nrow = 1, ncol = K),
      interval_constructor = interval_constructor,
      conformity_score = conformity_score_absolute_error
    )
  }

  return(object)
}

# Pinball loss function used in DtACI method
#
# alpha: coverage level: targets (1 - alpha) * 100\% prediction interval
# beta: largest value of theta such that Y is included in the prediction interval
# theta: current value of theta parameter
#
loss_dtaci <- function(alpha, beta, theta) {
  (1 - alpha) * (beta - theta) - pmin(0, beta - theta)
}

# Update DtACI with new data
update_dtaci <- function(object, Y, predictions, X = NULL, training = FALSE) {
  n <- length(Y)

  if(training == TRUE) {
    object$Y           <- c(object$Y, Y)
    object$predictions <- rbind(object$predictions, predictions)
    object$covered     <- c(object$covered, rep(NA, length(Y)))
    object$intervals   <- rbind(object$intervals, matrix(rep(NA, 2 * length(Y)), ncol = 2, nrow = length(Y)))
  }
  else {
    start_index <- nrow(object$internal$theta)
    object$internal$candidate_acis <- lapply(object$internal$candidate_acis, update.aci, newY = Y, newpredictions = predictions, newX = X, training = training)

    for(index in 1:n) {
      if(!is.null(object$parameters$eta)) {
        eta <- object$parameters$eta
      }
      else if(object$parameters$interval_constructor == "conformal") {
        alpha2 <- 1 - object$alpha
        denom = ((1 - alpha2)^2 * alpha2^3 + alpha2^2 * (1 - alpha2)^3) / 3
        eta <- sqrt(3 / object$parameters$I) * sqrt((log(object$parameters$I * object$internal$K) + 2) / denom)
      }
      else {
        loss_sq <- object$internal$losses[max(1, start_index + index - object$parameters$I):(start_index + index - 1)]^2
        if(length(object$internal$losses) == 0) {
          loss_sq_sum <- object$parameters$I * object$alpha^2
        }
        else {
          loss_sq_sum <- sum(loss_sq) * object$parameters$I / length(loss_sq)
        }
        if(loss_sq_sum == 0) {
          eta <- sqrt((log(object$internal$K * object$parameters$I) + 2) / 1)
        }
        else {
          eta <- sqrt((log(object$internal$K * object$parameters$I) + 2) / loss_sq_sum)
        }
      }

      weights <- object$internal$weights[start_index + index, ]
      p <- weights / sum(weights)
      thetas <- numeric(object$internal$K)
      for(k in 1:object$internal$K) {
        thetas[k] <- object$internal$candidate_acis[[k]]$internal$theta[start_index + index]
      }
      thetabar <- sum(thetas * p)

      # Generate a prediction interval before seeing the next observation
      interval <- object$internal$interval_constructor(predictions[index], thetabar, object)

      # Check if observation was inside or outside of the prediction interval
      covered <- Y[index] >= interval[1] && Y[index] <= interval[2]

      # Compute beta (beta is the largest value of theta such that Y is within the prediction interval)
      # For Y to be in the set, then S_alpha must be at least |prediction - Y|
      if(object$parameters$interval_constructor == "linear") {
        beta <- abs(predictions[index] - Y[index])
      }
      else if(object$parameters$interval_constructor == "conformal") {
        if(length(object$Y) == 0) {
          beta <- 0
        }
        else {
          S_alpha_target <- abs(predictions[index] - Y[index])
          scores <- object$internal$conformity_score(object$Y, object$prediction)
          beta <- mean(scores <= S_alpha_target)
        }
      }

      weights_bar <- numeric(object$internal$K)
      losses <- numeric(object$internal$K)
      for(k in 1:object$internal$K) {
        losses[k] <- loss_dtaci(1 - object$alpha, beta, thetas[k])
        weights_bar[k] <- weights[k] * exp(-eta * losses[k])
      }

      W_bar <- sum(weights_bar)
      weights_star <- (1 - object$parameters$sigma) * weights_bar + W_bar * object$parameters$sigma / object$internal$K

      object$internal$theta   <- rbind(object$internal$theta, thetabar)
      object$internal$weights <- rbind(object$internal$weights, weights_star / sum(weights_star))
      object$intervals        <- base::rbind(object$intervals, interval)
      object$Y                <- c(object$Y, Y[index])
      object$covered          <- c(object$covered, covered)
      object$predictions      <- base::rbind(object$predictions, predictions[index,])
      object$internal$losses  <- base::rbind(object$internal$losses, loss_dtaci(object$alpha, beta, thetabar))

      if(!is.null(X)) {
        object$X <- rbind(object$X, X[index, ])
      }
    }
  }

  return(object)
}

# Generate a DtACI prediction interval
predict_dtaci <- function(object, prediction, X) {
  weights <- object$internal$weights[nrow(object$internal$weights), ]
  p <- weights / sum(weights)

  thetas <- numeric(object$internal$K)
  for(k in 1:object$internal$K) {
    thetas[k] <- tail(object$internal$candidate_acis[[k]]$internal$theta, 1)
  }
  thetabar <- sum(thetas * p)

  object$internal$interval_constructor(prediction, thetabar, object)
}
