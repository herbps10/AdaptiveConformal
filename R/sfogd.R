initialize_sfogd <- function(object) {
  default_parameters <- list(
    gamma = 0.01,
    interval_constructor = "linear",
    conformity_score = "absolute_error",
    conditional = FALSE
  )

  acceptable_parameters <- list(
    interval_constructor = c("conformal", "linear"),
    conformity_score = c("absolute_error"),
    conditional = c(FALSE, TRUE)
  )

  if(is.null(object$internal)) {
    object$parameters <- initialize_parameters(
      object$parameters,
      default_parameters,
      acceptable_parameters
    )

    object$internal$K <- ncol(object$X)

    if(tolower(object$parameters$interval_constructor) == "asymmetric") {
      ntheta <- 2
    }
    else {
      ntheta <- ifelse(object$parameters$conditional, object$internal$K, 1)
    }

    if(tolower(object$parameters$interval_constructor) == "conformal") {
      interval_constructor <- interval_constructor_conformity(object$parameters$conformity_score)
      theta0 <- ifelse(is.null(object$parameters$theta0), object$alpha, object$parameters$theta0)
    }
    else if(tolower(object$parameters$interval_constructor) == "linear") {
      interval_constructor <- interval_constructor_linear()
      if(is.null(object$parameters$theta0)) {
        theta0 <- rep(0, ntheta)
      }
      else {
        theta0 <- rep(object$parameters$theta0, length.out = ntheta)
      }
    }
    else if(tolower(object$parameters$interval_constructor) == "asymmetric") {
      interval_constructor <- interval_constructor_asymmetric()
      if(is.null(object$parameters$theta0)) {
        theta0 <- rep(0, ntheta)
      }
      else {
        theta0 <- rep(object$parameters$theta0, length.out = ntheta)
      }
    }

    object$internal <- list(
      theta = matrix(theta0, ncol = ntheta),
      gradient = matrix(ncol = ntheta, nrow = 0),
      interval_constructor = interval_constructor
    )
  }

  return(object)
}

update_sfogd <- function(object, Y, predictions, X = NULL, training = FALSE) {
  n <- length(Y)
  prediction_matrix <- is.matrix(predictions)

  if(training == TRUE) {
    object$Y <- c(object$Y, Y)
    object$X <- rbind(object$X, X)
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
        interval <- predict_sfogd(object, predictions[index, ], X[index,])
      }
      else {
        interval <- predict_sfogd(object, predictions[index], X[index,])
      }

      # Check if observation was inside or outside of the prediction interval
      covered <- Y[index] >= interval[1] && Y[index] <= interval[2]
      below <- Y[index] < interval[1]
      above <- Y[index] > interval[2]

      object$intervals      <- base::rbind(object$intervals, interval)
      object$Y              <- c(object$Y, Y[index])
      if(!is.null(X)) {
        object$X              <- rbind(object$X, X[index,])
      }
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
        theta_star <- object$internal$theta[nrow(object$internal$theta),] -  object$parameters$gamma * gradient / sqrt(sum(object$gradient^2))
      }
      else {
        design_matrix <- rep(1, ncol(object$internal$theta))
        if(object$parameters$conditional && !is.null(X[index,])) {
          design_matrix <- X[index,]
        }
        theta_star <- pmax(0, object$internal$theta[nrow(object$internal$theta),] - (design_matrix == 1) * object$parameters$gamma * gradient / sqrt(sum(object$gradient^2)))
      }

      object$internal$theta <- rbind(object$internal$theta, theta_star)
    }
  }


  return(object)
}

# Generate a prediction interval
predict_sfogd <- function(object, prediction, X = NULL) {
  theta <- object$internal$theta[nrow(object$internal$theta),]
  if(object$parameters$conditional) {
    theta <- X %*% object$internal$theta[nrow(object$internal$theta), ]
  }
  object$internal$interval_constructor(prediction, theta, object)
}
