initialize_sfogd <- function(object) {
  default_parameters <- list(
    gamma = 0.01,
    interval_constructor = "linear",
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
    else if(tolower(object$parameters$interval_constructor) == "asymmetric") {
      interval_constructor <- interval_constructor_asymmetric()
      if(is.null(object$parameters$theta0)) {
        theta0 <- c(0, 0)
      }
      else {
        theta0 <- rep(object$parameters$theta0, length.out = 2)
      }
    }

    object$internal <- list(
      theta = matrix(theta0, ncol = length(theta0)),
      gradient = matrix(ncol = length(theta0), nrow = 0),
      interval_constructor = interval_constructor
    )
  }

  return(object)
}

update_sfogd <- function(object, Y, predictions, training = FALSE) {
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
        interval <- predict_sfogd(object, predictions[index, ])
      }
      else {
        interval <- predict_sfogd(object, predictions[index])
      }

      # Check if observation was inside or outside of the prediction interval
      covered <- Y[index] >= interval[1] && Y[index] <= interval[2]
      below <- Y[index] < interval[1]
      above <- Y[index] > interval[2]

      object$intervals      <- base::rbind(object$intervals, interval)
      object$Y              <- c(object$Y, Y[index])
      object$covered        <- c(object$covered, covered)
      if(prediction_matrix) {
        object$predictions  <- base::rbind(object$predictions, predictions[index,])
      }
      else {
        object$predictions  <- base::rbind(object$predictions, predictions[index])
      }

      if(object$parameters$interval_constructor == "asymmetric") {
        gradient <- c(
          1 - 2/(1 - object$alpha) * below,
          1 - 2/(1 - object$alpha) * above
        )
      }
      else {
        gradient <- covered - object$alpha
      }
      object$gradient <- rbind(object$gradient, gradient)

      # Update theta
      if(object$parameters$interval_constructor == "asymmetric") {
        theta_star <- object$internal$theta[nrow(object$internal$theta),] - object$parameters$gamma * gradient / sqrt(sum(object$gradient^2))
      }
      else {
        theta_star <- pmax(0, object$internal$theta[nrow(object$internal$theta),] - object$parameters$gamma * gradient / sqrt(sum(object$gradient^2)))
      }

      object$internal$theta <- rbind(object$internal$theta, theta_star)
    }
  }


  return(object)
}

# Generate a prediction interval
predict_sfogd <- function(object, prediction) {
  object$internal$interval_constructor(prediction, object$internal$theta[nrow(object$internal$theta),], object)
}
