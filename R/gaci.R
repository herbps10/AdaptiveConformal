initialize_gaci <- function(object) {
  default_parameters <- list(
    interval_constructor = "linear",
    conformity_score = "absolute_error",
    online_algorithm = "FTL",
    gamma = 0.01,
    model_matrix = NULL,
    lambda = 1
  )

  if(is.null(object$internal)) {
    for(n in names(default_parameters)) {
      if(is.null(object$parameters[[n]])) {
        object$parameters[[n]] <- default_parameters[[n]]
      }
    }

    if(tolower(object$parameters$interval_constructor) == "asymmetric") {
      ntheta <- 2
    }
    else {
      ntheta <- ifelse(is.null(object$parameters$design_matrix), 1, ncol(object$parameters$design_matrix))
    }

    gamma <- object$parameters$gamma
    if(length(object$parameters$gamma) != ntheta) {
      gamma <- rep(object$parameters$gamma, ntheta)
    }

    if(tolower(object$parameters$interval_constructor) == "conformal") {
      interval_constructor <- interval_constructor_conformity(object$parameters$conformity_score)
      theta0 <- c(ifelse(is.null(object$parameters$theta0), object$alpha, object$parameters$theta0), rep(0, ntheta - 1))
    }
    else if(tolower(object$parameters$interval_constructor) == "linear") {
      interval_constructor <- interval_constructor_linear()
      theta0 <- rep(ifelse(is.null(object$parameters$theta0), 0, object$parameters$theta0), ntheta)
    }
    else if(tolower(object$parameters$interval_constructor) == "asymmetric") {
      interval_constructor <- interval_constructor_asymmetric()
      theta0 <- rep(ifelse(is.null(object$parameters$theta0), 0, object$parameters$theta0), ntheta)
    }


    object$internal <- list(
      theta = matrix(theta0, ncol = ntheta, byrow = TRUE),
      interval_constructor = interval_constructor,
      ntheta = ntheta,
      gamma = gamma
    )
  }

  return(object)
}

update_gaci <- function(object, Y, predictions, training = FALSE) {
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
    object$covered   <- c(object$covered, rep(na, length(Y)))
    object$intervals <- rbind(object$intervals, matrix(rep(na, 2 * length(Y)), ncol = 2, nrow = length(Y)))
  }
  else {
    for(index in 1:n) {
      # generate a prediction interval
      if(prediction_matrix) {
        pred <- predictions[index, ]
      }
      else {
        pred <- predictions[index]
      }

      if(is.null(object$parameters$design_matrix)) {
        design <- NULL
      }
      else {
        design <- object$parameters$design_matrix[index, ]
      }
      interval <- predict_gaci(object, pred, design)

      # check if observation was inside or outside of the prediction interval
      below <- Y[index] < interval[1]
      above <- Y[index] > interval[2]
      covered <- Y[index] >= interval[1] && Y[index] <= interval[2]

      object$intervals      <- base::rbind(object$intervals, interval)
      object$Y              <- c(object$Y, Y[index])
      object$covered        <- c(object$covered, covered)
      if(prediction_matrix) {
        object$predictions  <- base::rbind(object$predictions, predictions[index,])
      }
      else {
        object$predictions  <- base::rbind(object$predictions, predictions[index])
      }

      # Update theta

      # Follow the Leader
      if(object$parameters$online_algorithm == "FTL" || object$parameters$online_algorithm == "FTRL") {
        gaci_empirical_risk <- function(theta) {
          l <- 0
          if(object$parameters$online_algorithm == "FTRL") {
            l <- l + object$parameters$lambda * sum((theta - object$internal$theta[nrow(object$internal$theta), ])^2)
          }

          if(!is.null(object$parameters$design_matrix)) {
            theta <- object$parameters$design_matrix[1:length(object$Y),] %*% theta
          }
          intervals <- matrix(object$internal$interval_constructor(object$predictions, theta, object), ncol = 2)

          l <- l + sum(interval_loss(object$Y, intervals[, 1], intervals[, 2], object$alpha))

          # Can we do better by trying to predict the next loss?
          # (most of the time, Y will fall within the interval)
          #if(object$parameters$ftrl == TRUE) {
          #  l <- l + 2 * theta
          #}

          l
        }

        if(length(object$Y) == 0) {
          theta_star <- ifelse(object$parameters$interval_constructor == "conformal", object$alpha, rep(0, object$internal$ntheta))
        }
        else {
          if(object$internal$ntheta == 1) {
            if(object$parameters$interval_constructor == "conformal") {
              lower <- 0
              upper <- 1
            }
            else {
              upper <- 5
              if(prediction_matrix && ncol(object$predictions) == 2) {
                lower <- min((object$predictions[,1] - object$predictions[,2]) / 2)
              }
              else if(prediction_matrix && ncol(object$predictions) == 1) {
                upper <- max(object$Y - object$predictions[,1])
                lower <- 0
              }
              else {
                upper <- max(object$Y - object$predictions)
                lower <- 0
              }
            }
            theta_star <- optimize(gaci_empirical_risk, interval = c(lower, upper))$minimum
          }
          else {
            theta0 <- rep(0, object$internal$ntheta)
            #if(index == 800) browser()
            theta_star <- optim(theta0, fn = gaci_empirical_risk)$par
          }
        }
      }
      # Online Gradient Descent
      else if(object$parameters$online_algorithm == "OGD") {
        if(!is.null(object$parameters$design_matrix)) {
          active <- which(object$parameters$design_matrix[index, ] == 1)
          theta_star <- object$internal$theta[nrow(object$internal$theta), ]
          theta_star[active] <- theta_star[active] + object$internal$gamma[active] * (1 - covered - (1 - object$alpha))
        }
        else {
          if(object$parameters$interval_constructor == "asymmetric") {
            theta_star <- numeric(2)
            theta_star[1] <- object$internal$theta[nrow(object$internal$theta), 1] + object$internal$gamma[1] * (below - (1 - object$alpha) / 2)
            theta_star[2] <- object$internal$theta[nrow(object$internal$theta), 2] + object$internal$gamma[2] * (above - (1 - object$alpha) / 2)
          }
          else {
            theta_star <- object$internal$theta[nrow(object$internal$theta), ] + object$internal$gamma * (1 - covered - (1 - object$alpha))
          }
        }
      }

      # Save updated theta
      object$internal$theta <- rbind(object$internal$theta, theta_star)
    }
  }


  return(object)
}

# generate a prediction interval
predict_gaci <- function(object, prediction, design_matrix = NULL) {
  if(!is.null(design_matrix)) {
    object$internal$interval_constructor(prediction, design_matrix %*% object$internal$theta[nrow(object$interna$theta), ], object)
  }
  else if(object$internal$ntheta > 1) {
    object$internal$interval_constructor(prediction, object$internal$theta[nrow(object$interna$theta), ], object)
  }
  else {
    object$internal$interval_constructor(prediction, tail(object$internal$theta, 1), object)
  }
}

#' @importFrom utils tail
gaci_oracle <- function(object) {
  oracle <- object
  # retrieve the last theta
  theta_star <- oracle$internal$theta[nrow(oracle$internal$theta), ]
  oracle$internal$theta <- matrix(theta_star, nrow = nrow(oracle$internal$theta), ncol = ncol(oracle$internal$theta), byrow = TRUE)

  theta <- oracle$internal$theta
  if(!is.null(object$parameters$design_matrix)) {
    theta <- object$parameters$design_matrix %*% theta_star
  }

  # regenerate all the intervals
  oracle$intervals <- oracle$internal$interval_constructor(object$predictions, theta, oracle)

  oracle$covered <- oracle$Y >= oracle$intervals[,1] & oracle$Y <= oracle$intervals[,2]
  oracle <- compute_metrics(oracle)

  oracle$method <- "GACI Oracle"

  oracle
}
