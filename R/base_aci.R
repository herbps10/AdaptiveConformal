initialize_aci <- function(object) {
  if(is.null(object$internal)) {
    # Default to using a interval constructor based on absolute error conformity scores
    if(is.null(object$parameters$conformity_score)) {
      object$parameters$conformity_score <- "absolute_error"
    }

    if(is.null(object$parameters$interval_constructor)) {
      object$parameters$interval_constructor <- "conformity"
    }

    if(tolower(object$parameters$interval_constructor) == "conformity") {
      interval_constructor <- interval_constructor_conformity(object$parameters$conformity_score)
    }
    else if(tolower(object$parameters$interval_constructor) == "linear") {
      interval_constructor <- interval_constructor_linear()
    }

    # Default value of theta is alpha
    theta0 <- ifelse(is.null(object$parameters$theta0), object$alpha, object$parameters$theta0)

    object$internal <- list(
      theta = c(theta0),
      interval_constructor = interval_constructor
    )
  }

  return(object)
}

update_aci <- function(object, Y, predictions, training = FALSE) {
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
        interval <- predict_aci(object, predictions[index, ])
      }
      else {
        interval <- predict_aci(object, predictions[index])
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

#' Generate a prediction interval
predict_aci <- function(object, prediction) {
  object$internal$interval_constructor(prediction, tail(object$internal$theta, 1), object)
}
