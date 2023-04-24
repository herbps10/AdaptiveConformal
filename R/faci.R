initialize_faci <- function(object) {
  if(is.null(object$internal)) {
    I <- 250

    if(is.null(object$parameters$interval_constructor)) {
      object$parameters$interval_constructor <- "conformity"
    }

    interval_constructor <- interval_constructor_conformity("absolute_error")

    if(is.null(object$parameters$gamma_grid)) {
      object$parameters$gamma_grid <- c(0.001, 0.002, 0.004, 0.008, 0.016, 0.032, 0.065, 0.128)
    }

    # K is the number of candidate gamma values
    K = length(object$parameters$gamma_grid)

    if(is.null(object$parameters$eta)) {
      alpha <- object$alpha
      object$parameters$eta <- sqrt(3 / I) * sqrt((log(K * I) + 2) / ((1 - alpha)^2 * alpha^3 + alpha^2 * (1 - alpha)^3))
    }

    if(is.null(object$parameters$sigma)) {
      object$parameters$sigma <- 1 / (2 * 500)
    }

    if(!is.null(object$parameters$theta0)) {
      if(K == 1) {
        theta0 <- rep(object$parameters$theta0, K)
      }
      else {
        if(length(object$parameters$theta0) != K) {
          stop("theta0 must have same length as gamma_grid")
        }
        theta0 <- object$parameters$theta0
      }
    }
    else {
      # Default value of theta is 1 - alpha
      theta0 <- rep(1 - object$alpha, K)
    }

    object$internal <- list(
      K = K,
      theta = matrix(theta0, nrow = 1, ncol = K),
      weights = matrix(rep(1, K), nrow = 1, ncol = K),
      interval_constructor = interval_constructor,
      conformity_score = conformity_score_absolute_error
    )
  }

  return(object)
}

loss_faci <- function(alpha, beta, theta) {
  alpha * (beta - theta) - pmin(0, beta - theta)
}

update_faci <- function(object, Y, predictions, training = FALSE) {
  n <- length(Y)
  prediction_matrix <- is.matrix(predictions)
  if(prediction_matrix) {
    stop("FACI currently does not support predictions in the form of a matrix.")
  }

  if(training == TRUE) {
    object$Y <- c(object$Y, Y)
    if(is.matrix(predictions)) {
      object$predictions <- rbind(object$predictions, predictions)
    }
    else {
      object$predictions <- rbind(object$predictions, t(t(predictions)))
    }
    object$covered   <- c(object$covered, rep(NA, length(Y)))
    object$intervals <- rbind(object$intervals, matrix(rep(NA, 2 * length(Y)), ncol = 2, nrow = length(Y)))
  }
  else {
    for(index in 1:n) {
      t_minus_one <- nrow(object$internal$weights)

      p <- object$internal$weights[t_minus_one, ]
      p <- p / sum(p)
      thetabar <- sum(object$internal$theta[t_minus_one, ] * p)

      # Generate a prediction interval before seeing the next observation
      interval <- predict_faci(object, predictions[index])

      # Check if observation was inside or outside of the prediction interval
      covered <- Y[index] >= interval[1] && Y[index] <= interval[2]

      # Check if each of the sublearners covered the prediction interval
      err <- unlist(lapply(object$internal$theta[t_minus_one, ], function(theta) {
        interval <- object$internal$interval_constructor(predictions[index], 1 - theta, object)
        Y[index] < interval[1] || Y[index] > interval[2]
      }))

      # Compute beta (beta is the largest value of theta such that Y is within the prediction interval)
      # For Y to be in the set, then S_alpha must be at least |prediction - Y|
      if(length(object$Y) == 0) {
        beta <- 0
      }
      else {
        S_alpha_target <- abs(predictions[index] - Y[index])
        scores <- object$internal$conformity_score(object$Y, object$prediction)
        beta <- mean(scores > S_alpha_target)
      }

      weights_bar <- numeric(object$internal$K)
      for(k in 1:object$internal$K) {
        weights_bar[k] <- object$internal$weights[t_minus_one, k] * exp(-object$parameters$eta * loss_faci(1 - object$alpha, beta, object$internal$theta[t_minus_one, k]))
      }

      W_bar <- sum(weights_bar)
      weights_star <- (1 - object$parameters$sigma) * weights_bar + W_bar * object$parameters$sigma / object$internal$K

      # Update theta
      theta_star <- thetabar + object$parameters$gamma_grid * (1 - object$alpha - err)

      object$internal$theta   <- rbind(object$internal$theta, theta_star)
      object$internal$weights <- rbind(object$internal$weights, weights_star)
      object$intervals        <- base::rbind(object$intervals, interval)
      object$Y                <- c(object$Y, Y[index])
      object$covered          <- c(object$covered, covered)
      object$predictions      <- base::rbind(object$predictions, predictions[index])
    }
  }


  return(object)
}

#' Generate a prediction interval
predict_faci <- function(object, prediction) {
  t <- nrow(object$internal$theta)
  p <- object$internal$weights[t, ]
  p <- p / sum(p)
  thetabar <- sum(object$internal$theta[t, ] * p)

  object$internal$interval_constructor(prediction, 1 - thetabar, object)
}
