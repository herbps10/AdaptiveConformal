initialize_rolling_rc <- function(object) {
  default_parameters <- list(
    gamma = 0.01,
    interval_constructor = "conformal",
    conformity_score = "absolute_error"
  )

  if(is.null(object$internal)) {
    for(n in names(default_parameters)) {
      if(is.null(object$parameters[[n]])) {
        object$parameters[[n]] <- default_parameters[[n]]
      }
    }

    if(tolower(object$parameters$interval_constructor) == "conformal") {
      interval_constructor <- interval_constructor_conformity(object$parameters$conformity_score)
      theta0 <- ifelse(is.null(object$parameters$theta0), object$alpha, object$parameters$theta0)
    }
    else if(tolower(object$parameters$interval_constructor) == "linear") {
      interval_constructor <- interval_constructor_linear()
      theta0 <- ifelse(is.null(object$parameters$theta0), 0, object$parameters$theta0)
    }

    object$internal <- list(
      theta = c(theta0),
      interval_constructor = interval_constructor
    )
  }

  return(object)
}

update_rolling_rc <- function(object, Y, predictions, training = FALSE) {
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
    object$covered   <- c(object$covered, rep(NA, length(Y)))
    object$intervals <- rbind(object$intervals, matrix(rep(NA, 2 * length(Y)), ncol = 2, nrow = length(Y)))
  }
  else {
    for(index in 1:n) {
      # Generate a prediction interval
      if(prediction_matrix) {
        interval <- predict_rolling_rc(object, predictions[index, ])
      }
      else {
        interval <- predict_rolling_rc(object, predictions[index])
      }

      # Check if observation was inside or outside of the prediction interval
      covered <- Y[index] >= interval[1] && Y[index] <= interval[2]

      # Update theta
      theta_star <- tail(object$internal$theta, 1) + object$parameters$gamma * (1 - covered - (1 - object$alpha))

      object$internal$theta <- c(object$internal$theta, theta_star)
      object$intervals      <- base::rbind(object$intervals, interval)
      object$Y              <- c(object$Y, Y[index])
      object$covered        <- c(object$covered, covered)
      object$predictions    <- base::rbind(object$predictions, predictions[index])
    }
  }


  return(object)
}

# Generate a prediction interval
predict_rolling_rc <- function(object, prediction) {
  object$internal$interval_constructor(prediction, tail(object$internal$theta, 1), object)
}
