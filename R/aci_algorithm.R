initialize_aci <- function(object) {
  default_parameters <- list(
    gamma = 0.01,
    interval_constructor = "conformal",
    conformity_score = "absolute_error",
    symmetric = TRUE,
    conditional = FALSE
  )

  acceptable_parameters <- list(
    interval_constructor = c("conformal", "linear"),
    conformity_score = c("absolute_error"),
    symmetric = c(FALSE, TRUE),
    conditional = c(FALSE, TRUE)
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

    object$internal <- list(
      theta = matrix(theta0, ncol = ntheta),
      interval_constructor = interval_constructor
    )
  }

  return(object)
}

update_aci <- function(object, Y, predictions, X = NULL, training = FALSE) {
  n <- length(Y)

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
      interval <- predict_aci(object, predictions[index, ], X[index, ])

      # Check if observation was inside or outside of the prediction interval
      below <- Y[index] < interval[1]
      above <- Y[index] > interval[2]
      covered <- Y[index] >= interval[1] && Y[index] <= interval[2]

      # Update theta
      if(object$parameters$symmetric == TRUE) {
        if(object$parameters$conditional) {
          theta_star <- object$internal$theta[nrow(object$internal$theta), ] + X[index, ] * object$parameters$gamma * (1 - covered - (1 - object$alpha))
        }
        else {
          theta_star <- object$internal$theta[nrow(object$internal$theta), ] + object$parameters$gamma * (1 - covered - (1 - object$alpha))
        }
      }
      else {
        if(object$parameters$conditional) {
          theta_star <- numeric(ncol(object$internal$theta))
          theta_star[1:ncol(X)] <- object$internal$theta[nrow(object$internal$theta), 1:ncol(X)] + X[index, ] * object$parameters$gamma * (below - (1 - object$alpha) / 2)
          theta_star[(ncol(X) + 1):ncol(object$internal$theta)] <- object$internal$theta[nrow(object$internal$theta), (ncol(X) + 1):ncol(object$internal$theta)] + X[index, ] * object$parameters$gamma * (above - (1 - object$alpha) / 2)
        }
        else {
          theta_star <- object$internal$theta[nrow(object$internal$theta), ] + object$parameters$gamma * c(
            (below - (1 - object$alpha) / 2),
            (above - (1 - object$alpha) / 2)
          )
        }
      }

      object$internal$theta <- base::rbind(object$internal$theta, theta_star)
      object$intervals      <- base::rbind(object$intervals, interval)
      object$Y              <- c(object$Y, Y[index])

      if(!is.null(X)) object$X <- rbind(object$X, X[index, ])
      object$covered        <- c(object$covered, covered)

      object$predictions  <- base::rbind(object$predictions, predictions[index,])
    }
  }

  object$internal$theta <- unname(object$internal$theta)

  return(object)
}

# Generate a prediction interval
predict_aci <- function(object, prediction, X = NULL) {
  if(is.vector(X)) {
    X <- matrix(X, ncol = length(X))
  }

  theta <- object$internal$theta[nrow(object$internal$theta), ]
  if(object$parameters$conditional == TRUE) {
    if(object$parameters$interval_constructor == "asymmetric") {
      theta <- c(X %*% theta[1:ncol(X)], X %*% theta[(ncol(X) + 1):length(theta)])
    }
    else {
      theta <- X %*% theta
    }
  }
  object$internal$interval_constructor(prediction, theta, object)
}
