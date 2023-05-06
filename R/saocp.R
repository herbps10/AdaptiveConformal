initialize_saocp <- function(object) {
  default_parameters <- list(
    D = 1,
    g = 8,
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
    }
    else if(tolower(object$parameters$interval_constructor) == "linear") {
      interval_constructor <- interval_constructor_linear()
    }

    object$internal <- list(
      experts = list(),
      interval_constructor = interval_constructor
    )
  }

  return(object)
}

update_saocp <- function(object, Y, predictions, training = FALSE) {
  L <- function(t, multiplier) {
        n <- 0
        while(t %% 2 == 0) {
          t <- t / 2
          n <- n + 1
        }
        multiplier * 2^n
  }

  score <- conformity_score_absolute_error

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
      t <- length(object$Y) + 1

      # Initialize a new expert
      num_experts <- length(object$internal$experts) + 1
      object$internal$experts[[num_experts]] <- aci(alpha = object$alpha, method = "SF-OGD", parameters = list(
        interval_constructor = object$parameters$interval_constructor,
        gamma = object$parameters$D / sqrt(3),
        theta0 = ifelse(length(object$internal$theta > 0), saocp_theta(object), 0)
      ))
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
      object$internal$theta <- c(object$internal$theta, theta_star)

      # Generate a prediction interval
      if(prediction_matrix) {
        interval <- predict_saocp(object, predictions[index, ])
      }
      else {
        interval <- predict_saocp(object, predictions[index])
      }
      object$intervals      <- base::rbind(object$intervals, interval)

      # Check if observation was inside or outside of the prediction interval
      covered <- Y[index] >= interval[1] && Y[index] <= interval[2]# Generate a prediction interval

      conformity_scores <- score(object$predictions, object$Y)
      if(length(conformity_scores) == 0) {
        radius <- 0
      }
      else {
        #radius <- mean(score(predictions[index], Y[index]) >= conformity_scores)
        radius <- abs(Y[index] - predictions[index])
      }

      # Save predictions
      if(prediction_matrix) {
        object$predictions  <- base::rbind(object$predictions, predictions[index,])
      }
      else {
        object$predictions  <- base::rbind(object$predictions, predictions[index])
      }

      # Updates
      #if(t == 100) browser()
      theta_star_loss <- max(object$alpha * (radius - theta_star), (1 - object$alpha) * (theta_star - radius))

      # Update weights
      object$internal$experts <- lapply(object$internal$experts, function(expert) {
        expert_theta <- tail(expert$internal$theta, 1)
        expert_loss <- max(object$alpha * (radius - expert_theta), (1 - object$alpha) * (expert_theta - radius))

        g <- 1 / object$parameters$D * ifelse(expert$internal$weight > 0,
                                              min(max(-1, theta_star_loss - expert_loss), 1),
                                              min(max(0, theta_star_loss - expert_loss), 1))
        g <- g / max(object$alpha, 1 - object$alpha)

        expert$internal$sum_g <- expert$internal$sum_g + g
        expert$internal$sum_weights_g <- expert$internal$sum_weights_g + expert$internal$weight * g

        # Compute weight for time t + 1
        expert$internal$weight <- 1 / (t - expert$internal$start_time + 1) * expert$internal$sum_g * (1 + expert$internal$sum_weights_g)

        # Update expert
        if(prediction_matrix) {
          expert <- update.aci(expert, Y[index], predictions[index, ])
        }
        else {
          expert <- update.aci(expert, Y[index], predictions[index])
        }

        expert
      })

      object$Y              <- c(object$Y, Y[index])
      object$covered        <- c(object$covered, covered)
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

  thetas <- unlist(lapply(object$internal$experts, function(x) tail(x$internal$theta, 1)))
  if(length(thetas) == 0) {
    theta_star <- 0
  }
  else {
    theta_star <- sum(phat * thetas)
  }
  theta_star
}

# Generate a prediction interval
predict_saocp <- function(object, prediction) {
  object$internal$interval_constructor(prediction, saocp_theta(object), object)
}
