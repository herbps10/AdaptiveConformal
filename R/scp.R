initialize_scp <- function(object) {
  default_parameters <- list(
    gamma = 0.01,
    interval_constructor = "conformal",
    conformity_score = "absolute_error",
    symmetric = TRUE,
    conditional = FALSE
  )

  acceptable_parameters <- list(
    interval_constructor = c("conformal"),
    conformity_score = c("absolute_error"),
    symmetric = c(FALSE, TRUE),
    conditional = c(FALSE)
  )

  if(is.null(object$internal)) {
    object$parameters <- initialize_parameters(
      object$parameters,
      default_parameters,
      acceptable_parameters
    )

    if(object$parameters$interval_constructor == "conformal") {
      interval_constructor <- interval_constructor_conformity(object$parameters$conformity_score, object$parameters$symmetric)
    }

    object$internal <- list(
      interval_constructor = interval_constructor
    )
  }

  return(object)
}

update_scp <- function(object, Y, predictions, X = NULL, training = FALSE) {
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
      interval <- predict_scp(object, predictions[index, ], X[index,])

      # Check if observation was inside or outside of the prediction interval
      covered <- Y[index] >= interval[1] && Y[index] <= interval[2]
      below <- Y[index] < interval[1]
      above <- Y[index] > interval[2]

      object$intervals   <- base::rbind(object$intervals, interval)
      object$Y           <- c(object$Y, Y[index])
      object$covered     <- c(object$covered, covered)
      object$predictions <- base::rbind(object$predictions, predictions[index,])

      if(!is.null(X)) {
        object$X <- rbind(object$X, X[index,])
      }
    }
  }

  return(object)
}

# Generate a prediction interval
predict_scp <- function(object, prediction, X = NULL) {
  object$internal$interval_constructor(prediction, object$alpha, object)
}
