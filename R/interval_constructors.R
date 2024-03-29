#' @importFrom stats quantile
interval_constructor_conformity <- function(conformity_score, symmetric = TRUE) {
  if(is.function(conformity_score)) {
    score <- conformity_score
  }
  else if(conformity_score == "absolute_error") {
    score <- conformity_score_absolute_error
  }

  if(symmetric == TRUE) {
    function(prediction, theta, object) {
      conformity_scores <- score(object$predictions, object$Y)
      Salpha <- quantile(conformity_scores, pmax(0, pmin(theta, 1)), type = 1, na.rm = TRUE)
      Salpha <- ifelse(is.na(Salpha), 0, Salpha)
      if(is.matrix(prediction)) {
        intervals <- matrix(ncol = 2, nrow = length(object$Y))
        intervals[, 1] <- prediction[, 1] - Salpha
        intervals[, 2] <- prediction[, 1] + Salpha
        intervals
      }
      else {
        c(prediction - Salpha, prediction + Salpha)
      }
    }
  }
  else {
    function(prediction, theta, object) {
      #conformity_scores_lower <- score(object$predictions, object$Y)[object$predictions < object$Y]
      #conformity_scores_upper <- score(object$predictions, object$Y)[object$predictions > object$Y]
      conformity_scores <- object$Y - object$predictions

      Salpha_lower <- quantile(conformity_scores, pmax(0, pmin((1 - theta) / 2, 1)), type = 1, na.rm = TRUE)
      Salpha_lower <- ifelse(is.na(Salpha_lower), 0, Salpha_lower)

      Salpha_upper <- quantile(conformity_scores,  pmax(0, pmin(1 - (1 - theta) / 2, 1)), type = 1, na.rm = TRUE)
      Salpha_upper <- ifelse(is.na(Salpha_upper), 0, Salpha_upper)

      if(is.matrix(prediction)) {
        intervals <- matrix(ncol = 2, nrow = length(object$Y))
        intervals[, 1] <- prediction[, 1] + Salpha_lower
        intervals[, 2] <- prediction[, 1] + Salpha_upper
        intervals
      }
      else {
        c(prediction + Salpha_lower, prediction + Salpha_upper)
      }
    }
  }
}

interval_constructor_linear <- function(symmetric) {
  if(symmetric == TRUE) {
    function(prediction, theta, object) {
      if(is.matrix(prediction) && ncol(prediction) == 2) {
        intervals <- prediction
        intervals[,1] <- prediction[,1] - theta
        intervals[,2] <- prediction[,2] + theta

        # Deal with malformed intervals (lower bound is greater than upper bound)
        malformed <- intervals[,1] > intervals[,2]
        center <- (intervals[,2] + intervals[,1]) / 2
        intervals[malformed, 1] <- center[malformed]
        intervals[malformed, 2] <- center[malformed]

        intervals
      }
      else if(is.matrix(prediction) && ncol(prediction) == 1) {
        intervals <- matrix(ncol = 2, nrow = nrow(prediction))
        intervals[, 1] <- prediction[, 1] - theta
        intervals[, 2] <- prediction[, 1] + theta

        intervals
      }
      else if(length(prediction) == 1) {
        if(theta < 0) {
          return(c(prediction[1], prediction[1]))
        }
        return(c(prediction[1] - theta, prediction[1] + theta))
      }
      else if(length(prediction) == 2) {
        if((prediction[1] - theta) > (prediction[2] + theta)) {
          theta <- (prediction[1] - prediction[2]) / 2
        }
        return(c(prediction[1] - theta, prediction[2] + theta))
      }
    }
  }
  else {
    function(prediction, theta, object) {
      if(is.matrix(prediction) && ncol(prediction) == 2) {
        intervals <- prediction
        intervals[,1] <- prediction[,1] - theta[1]
        intervals[,2] <- prediction[,2] + theta[2]

        # Deal with malformed intervals (lower bound is greater than upper bound)
        malformed <- intervals[,1] > intervals[,2]
        center <- (intervals[,2] + intervals[,1]) / 2
        intervals[malformed, 1] <- center[malformed]
        intervals[malformed, 2] <- center[malformed]

        intervals
      }
      else if(is.matrix(prediction) && ncol(prediction) == 1) {
        intervals <- matrix(ncol = 2, nrow = nrow(prediction))
        intervals[, 1] <- prediction[, 1] - theta[1]
        intervals[, 2] <- prediction[, 1] + theta[2]

        intervals
      }
      else if(length(prediction) == 1) {
        #if((prediction[1] - theta[1]) > (prediction[1] + theta[2])) {
        #  return(c(prediction[1], prediction[1]))
        #}
        return(c(prediction[1] - theta[1], prediction[1] + theta[2]))
      }
      else if(length(prediction) == 2) {
        if((prediction[1] - theta[1]) > (prediction[2] + theta[2])) {
          theta <- rep((prediction[1] - prediction[2]) / 2, 2)
        }
        return(c(prediction[1] - theta[1], prediction[2] + theta[1]))
      }
    }
  }
}

#' @importFrom stats quantile
interval_constructor_recenter <- function() {
  function(prediction, theta, object) {
    if(is.matrix(prediction)) {
      intervals <- matrix(ncol = 2, nrow = length(object$Y))
      intervals[, 1] <- prediction[, 1] + theta[1] - theta[2]
      intervals[, 2] <- prediction[, 1] + theta[1] + theta[2]
      intervals
    }
    else {
      c(prediction + theta[1] - theta[2], prediction + theta[1] + theta[2])
    }
  }
}

