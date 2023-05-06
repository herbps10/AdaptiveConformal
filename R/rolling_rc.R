initialize_rolling_rc <- function(object) {
  default_parameters <- list(
    gamma = 0.01,
    interval_constructor = "conformal",
    conformity_score = "absolute_error",
    conditional = FALSE
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
      theta = matrix(theta0, ncol = 1),
      interval_constructor = interval_constructor
    )
  }

  return(object)
}

update_rolling_rc <- function(object, Y, predictions, X = NULL, training = FALSE) {
  n <- length(Y)
  prediction_matrix <- is.matrix(predictions)

  if(training == TRUE) {
    object$Y <- c(object$Y, Y)
    object$X <- rbind(object$X, X)
    object$predictions <- rbind(object$predictions, predictions)
    object$covered   <- c(object$covered, rep(NA, length(Y)))
    object$intervals <- rbind(object$intervals, matrix(rep(NA, 2 * length(Y)), ncol = 2, nrow = length(Y)))
  }
  else {
    for(index in 1:n) {
      # Generate a prediction interval
      interval <- predict_rolling_rc(object, predictions[index, ])

      # Check if observation was inside or outside of the prediction interval
      covered <- Y[index] >= interval[1] && Y[index] <= interval[2]

      # Update theta
      theta_star <- object$internal$theta[nrow(object$internal$theta), ] + object$parameters$gamma * (1 - covered - (1 - object$alpha))

      object$internal$theta <- rbind(object$internal$theta, theta_star)
      object$intervals      <- base::rbind(object$intervals, interval)
      object$Y              <- c(object$Y, Y[index])

      if(!is.null(X)) object$X <- rbind(object$X, X[index, ])
      object$covered        <- c(object$covered, covered)

      object$predictions  <- base::rbind(object$predictions, predictions[index,])
    }
  }

  return(object)
}

# Generate a prediction interval
predict_rolling_rc <- function(object, prediction, X = NULL) {
  object$internal$interval_constructor(prediction, object$internal$theta[nrow(object$internal$theta), ], object)
}
