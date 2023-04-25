interval_constructor_conformity <- function(conformity_score) {
  if(is.function(conformity_score)) {
    score <- conformity_score
  }
  else if(conformity_score == "absolute_error") {
    score <- conformity_score_absolute_error
  }

  function(prediction, theta, object) {
    conformity_scores <- score(object$predictions, object$Y)
    Salpha <- quantile(conformity_scores, max(0, min(theta, 1)), type = 1, na.rm = TRUE)
    Salpha <- ifelse(is.na(Salpha), 0, Salpha)
    c(prediction[1] - Salpha, prediction[1] + Salpha)
  }
}

interval_constructor_linear <- function() {
  function(prediction, theta, object) {
    if(length(prediction) == 1) {
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
