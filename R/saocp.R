initialize_saocp <- function(object) {
  default_parameters <- list(
    D = 1,
    g = 8,
    interval_constructor = "linear",
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

    object$internal$K <- ncol(object$X)

    if(object$parameters$symmetric == TRUE) {
      ntheta <- ifelse(object$parameters$conditional, object$internal$K, 1)
    }
    else {
      ntheta <- ifelse(object$parameters$conditional, 2 * object$internal$K, 2)
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

    score <- conformity_score_absolute_error

    object$internal <- list(
      experts = list(),
      theta0 = theta0,
      theta = matrix(nrow = 0, ncol = ntheta),
      interval_constructor = interval_constructor,
      score = score
    )
  }

  return(object)
}

update_saocp <- function(object, Y, predictions, X = NULL, training = FALSE) {
  L <- function(t, multiplier) {
        n <- 0
        while(t %% 2 == 0) {
          t <- t / 2
          n <- n + 1
        }
        multiplier * 2^n
  }

  n <- length(Y)

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
      t <- length(object$Y) + 1

      # Initialize a new expert
      num_experts <- length(object$internal$experts) + 1
      expert_theta0 <- object$internal$theta0
      if(nrow(object$internal$theta) > 0) {
        expert_theta0 <- saocp_theta(object)
      }

      object$internal$experts[[num_experts]] <- aci(
        alpha = object$alpha,
        method = "SF-OGD",
        X = matrix(ncol = ncol(object$X), nrow = 0),
        parameters = list(
          interval_constructor = object$parameters$interval_constructor,
          gamma = object$parameters$D / sqrt(3),
          theta0 = expert_theta0,
          conditional = object$parameters$conditional,
          symmetric = object$parameters$symmetric
        )
      )
      object$internal$experts[[num_experts]]$internal$weight <- 0
      object$internal$experts[[num_experts]]$internal$sum_g <- 0
      object$internal$experts[[num_experts]]$internal$sum_weights_g <- 0
      object$internal$experts[[num_experts]]$internal$start_time <- t
      object$internal$experts[[num_experts]]$internal$lifetime <- L(t, object$parameters$g)

      # Cull any experts that have expired
      object$internal$experts <- Filter( function(expert) {
        (expert$internal$start_time + expert$internal$lifetime) >= t
      }, object$internal$experts)

      # Compute theta
      theta_star <- saocp_theta(object)
      object$internal$theta <- rbind(object$internal$theta, theta_star)

      # Generate a prediction interval
      interval <- predict_saocp(object, predictions[index, ], X[index, ])
      object$intervals <- base::rbind(object$intervals, interval)

      # Check if observation was inside or outside of the prediction interval
      covered <- Y[index] >= interval[1] && Y[index] <= interval[2]
      below <- Y[index] < interval[1]
      above <- Y[index] > interval[2]

      conformity_scores <- object$internal$score(object$predictions, object$Y)
      if(length(conformity_scores) == 0) {
        radius <- 0
      }
      else {
        if(object$parameters$interval_constructor == "conformal") {
          radius <- mean(object$internal$score(predictions[index], Y[index]) >= conformity_scores)
        }
        else {
          radius <- abs(Y[index] - predictions[index])
        }
      }

      # Save predictions
      object$predictions <- base::rbind(object$predictions, predictions[index,])

      # Updates
      if(object$parameters$symmetric == TRUE) {
        if(object$parameters$conditional == TRUE) {
          theta_star <- X[index, ] %*% theta_star
        }
        theta_star_loss <- max(object$alpha * (radius - theta_star), (1 - object$alpha) * (theta_star - radius))
      }
      else {
        if(object$parameters$conditional == TRUE) {
          theta_star <- c(
            X[index, ] %*% theta_star[1:ncol(X)],
            X[index, ] %*% theta_star[(ncol(X) + 1):length(theta_star)]
          )
        }

        theta_star_loss <- theta_star[2] + theta_star[1] +
          2 / (1 - object$alpha) * (interval[1] - Y[index]) * below +
          2 / (1 - object$alpha) * (Y[index] - interval[2]) * above
      }

      # Update weights
      object$internal$experts <- lapply(object$internal$experts, function(expert) {
        expert_theta <- expert$internal$theta[nrow(expert$internal$theta),]
        #if(expert$internal$start_time == 138) browser()

        if(object$parameters$symmetric == TRUE) {
          if(object$parameters$conditional == TRUE) {
            expert_theta <- X[index, ] %*% expert_theta
          }
          expert_loss <- max(object$alpha * (radius - expert_theta), (1 - object$alpha) * (expert_theta - radius))
        }
        else {
          if(object$parameters$conditional == TRUE) {
            expert_theta <- c(
              X[index, ] %*% expert_theta[1:ncol(X)],
              X[index, ] %*% expert_theta[(ncol(X) + 1):length(expert_theta)]
            )
          }


          expert_below <- Y[index] < predictions[index] - expert_theta[1]
          expert_above <- Y[index] > predictions[index] + expert_theta[2]
          expert_loss <- expert_theta[2] + expert_theta[1] +
            2 / (1 - object$alpha) * ((predictions[index] - expert_theta[1]) - Y[index]) * expert_below +
            2 / (1 - object$alpha) * (Y[index] - (predictions[index] + expert_theta[2])) * expert_above
        }

        g <- 1 / object$parameters$D * ifelse(expert$internal$weight > 0,
                                              min(max(-1, theta_star_loss - expert_loss), 1),
                                              min(max(0, theta_star_loss - expert_loss), 1))
        g <- g / max(object$alpha, 1 - object$alpha)
        #g <- 1 / object$parameters$D * ifelse(expert$internal$weight > 0,
        #                                      theta_star_loss - expert_loss,
        #                                      max(0, theta_star_loss - expert_loss))
        #g <- g / max(object$alpha, 1 - object$alpha)


        expert$internal$sum_g <- expert$internal$sum_g + g
        expert$internal$sum_weights_g <- expert$internal$sum_weights_g + expert$internal$weight * g

        # Compute weight for time t + 1
        #if(expert$internal$start_time == 138) browser()

        expert$internal$weight <- 1 / (t - expert$internal$start_time + 1) * expert$internal$sum_g * (1 + expert$internal$sum_weights_g)

        # Update expert
        expert <- update.aci(expert, Y[index], predictions[index, ], newX = X[index, ])

        expert
      })

      object$Y <- c(object$Y, Y[index])
      if(!is.null(X)) {
        object$X       <- rbind(object$X, X[index, ])
      }
      object$covered <- c(object$covered, covered)
    }
  }

  return(object)
}

saocp_theta <- function(object) {
  ts <- unlist(lapply(object$internal$experts, function(x) x$internal$start_t))
  weights <- unlist(lapply(object$internal$experts, function(x) x$internal$weight))
  p <- ts^(-2) * (1 + floor(log2(ts)))^(-1)
  p <- p / sum(p)
  phat <- p * pmax(0, weights)
  if(sum(phat) > 0) {
    phat <- phat / sum(phat)
  }
  else {
    phat <- p
  }

  thetas <- matrix(unlist(lapply(object$internal$experts, function(x) {
    x$internal$theta[nrow(x$internal$theta),]
  })), ncol = ncol(object$internal$theta), byrow = TRUE)

  if(length(thetas) == 0) {
    theta_star <- rep(0, ncol(object$internal$theta))
  }
  else {
    theta_star <- phat %*% thetas
  }
  theta_star
}

# Generate a prediction interval
predict_saocp <- function(object, prediction, X = NULL) {
  if(is.vector(X)) X <- matrix(X, ncol = length(X))
  theta <- saocp_theta(object)
  if(object$parameters$conditional && !is.null(X)) {
    if(object$parameters$symmetric == FALSE) {
      theta <- c(
        X %*% theta[1:ncol(X)],
        X %*% theta[(ncol(X) + 1):length(theta)]
      )
    }
    else {
      theta <- X %*% t(theta)
    }
  }
  object$internal$interval_constructor(prediction, theta, object)
}
