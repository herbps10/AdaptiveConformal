#' Initialize an Adaptive Conformal Inference algorithm.
#'
#' @param Y optional vector of observations
#' @param predictions optional vector of predictions
#' @param training optional boolean indicating if the supplied Y and prediction values should be treated as training data (not used to update ACI parameters, but used in e.g. calculating conformity scores)
#' @param alpha desired level o
#' @param method a string specificying the Adaptive Conformal Inference method to use.
#' The available methods are:
#' \describe{
#'    \item{'ACI'}{The original Adaptive Conformal Inference method proposed by \insertCite{gibbs2021aci}.
#'    Requires specification of a positive learning rate \strong{gamma}.}
#'    \item{'AgACI'}{Aggregated ACI method introduced by \insertCite{zaffran2022agaci}.
#'    Multiple ACI algorithms are executed for a grid of learning rates, and the resulting intervals are combined using the Bernstein Online Aggregation method
#'    for online aggregation of experts. Requires specification of a grid of learning rates \strong{gamma_grid}.}
#' }
#' @param parameters a list of parameters that depend on the chosen ACI method.
#' \describe{
#'    \item{gamma}{A positive number specifying the learning rate. For method "ACI".}
#'    \item{gamma_grid}{A grid of positive learning rates. For method "AgACI".}
#'    \item{interval_constructor}{
#'      Specifies how the prediction intervals are to be formed.
#'      \begin{itemize}
#'        \item "linear": the interval is formed as [prediction - theta, prediction + theta].
#'        \item "conformal": the interval is formed as [prediction - S, prediction + S], where S is the theta*100% quantile of the previously observed
#'        conformity scores.
#'        \item A function that takes a prediction and a parameter theta (that is learned online) and returns a candidate prediction interval.
#'      \end{itemize}
#'    }
#'    \item{conformity_score}{
#'      Specifies the conformity scores to use for constructing prediction intervals. Only applicable if interval_constructor = "conformal".
#'      Current options are:
#'      \begin{align}
#'        \item "absolute_error": S = |Y - prediction|
#'        \item A function taking an observation Y and prediction and returning a conformity score.
#'      \end{align}
#'    }
#' }
#'
#' @export
aci <- function(Y = NULL, predictions = NULL, training = FALSE, alpha = 0.95, method = "ACI", parameters = list()) {
  method <- match.arg(method, c("AgACI", "ACI", "FACI"))

  object <- list(
    method = method,
    alpha = alpha,
    Y = NULL,
    predictions = NULL,
    intervals = matrix(ncol = 2, nrow = 0),
    covered = numeric(0),
    parameters = parameters,
    internal = NULL,
    coverage = NULL,
    training = logical(0)
  )

  initializers <- list(
    ACI = initialize_aci,
    AgACI = initialize_ag_aci,
    FACI = initialize_faci
  )

  object <- initializers[[method]](object)

  class(object) <- "aci"

  if(!is.null(Y) || !is.null(predictions)) {
    object <- update(object, newY = Y, newpredictions = predictions, training = training)
  }

  return(object)
}

#' Compute a conformal prediction interval
#'
#' @param object object of class "aci"
#' @param prediction vector or matrix of predictions to use for forming the conformal prediction interval
#'
#' @export
predict.aci <- function(object, prediction) {
  method <- match.arg(object$method, c("AgACI", "ACI", "FACI"))

  funs <- list(
    ACI = predict_aci,
    AgACI = predict_ag_aci,
    FACI = predict_faci
  )

  funs[[method]](object, prediction)
}


#' Print an ACI object
#'
#' @param object object of class "aci"
#'
#' @export
print.aci <- function(object) {
  observed <- object$training == FALSE
  N_intervals <- sum(observed)
  within <- sum(object$Y[observed] >= object$intervals[observed, 1] & object$Y[observed] <= object$intervals[observed, 2])

  cat(paste0("ACI object containing ", length(object$Y), " observations. Empirical coverage: ", format(signif(object$coverage * 100, 4)), "% (", within, "/", N_intervals, " prediction intervals contained the observed value)."))
}
