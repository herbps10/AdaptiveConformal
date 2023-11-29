#' Plot an ACI object.
#'
#' @param x object of class "aci"
#' @param index indexes of the observation and intervals to plot (defaults to showing all observations and intervals).
#' @param legend logical indicating whether or not to include a legend
#' @param engine either "base" or "ggplot"
#' @param ... additional arguments to base plot function
#'
#' @importFrom graphics plot lines legend
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme
#' @export
plot.aci <- function(x, index = NULL, legend = TRUE, predictions = TRUE, engine = "base", ...) {
  params <- list(...)

  if(is.null(index)) index = 1:length(x$Y)

  if(engine == "base") {
    col <- ifelse(is.na(x$covered[index]), "black", ifelse(x$covered[index], "darkblue", "darkred"))
    ylim <- range(c(x$Y[index], x$intervals[index, 1], x$intervals[index, 2]), na.rm = TRUE)

    if(is.null(params$ylim)) {
      plot(x$Y[index], ylim = ylim, col = col, xlab = "Index", ylab = "Y", ...)
    }
    else {
      plot(x$Y[index], col = col, xlab = "Index", ylab = "Y", ...)
    }

    lines(x$intervals[index, 1])
    lines(x$intervals[index, 2])

    legend_text <- c("Within interval", "Outside interval")
    legend_col <- c("darkblue", "darkred")
    legend_pch = c(1, 1)

    if(predictions == TRUE) {
      legend_text <- c(legend_text, "Predictions")
      legend_col <- c(legend_col, "green")
      legend_pch <- c(legend_pch, 45)
      for(i in 1:ncol(x$predictions)) {
        lines(x$predictions[index, i], col = "green", lty = 1, lwd = 1)
      }
    }


    if(legend == TRUE) {
      legend("topright", legend = legend_text, col = legend_col, pch = legend_pch)
    }
  }
  else if(engine == "ggplot") {
    p.data <- data.frame(
      x = 1:length(index),
      y = x$Y[index],
      lower = x$intervals[index, 1],
      upper = x$intervals[index, 2],
      predictions = x$predictions[index],
      covered = factor(x$covered[index], labels = c("Outside interval", "Within interval"))
    )

    p <- ggplot(p.data, aes(x = index, y = y)) +
      geom_point(aes(color = covered), size = 0.5) +
      geom_line(aes(y = predictions, color = "Predictions")) +
      geom_line(aes(y = lower), alpha = 0.5) +
      geom_line(aes(y = upper), alpha = 0.5)

    if(legend == FALSE) {
      p <- p + theme(legend.position = "none")
    }

    p
  }
}

#' Plot most recent weights assigned to each of the candidate ACI algorithms
#' by the AgACI method
#'
#' @param x object of class "aci"
#' @param type type of plot: "last" to show only most recent weights, "last" to show all weights
#' @param legend legend position
#' @param ... additional arguments to plot
#' @export
plot_agaci_weights <- function(x, type = "last", legend = "topright", ...) {
  if(x$method != "AgACI") {
    stop("Method only supported for ACI objects fit with AgACI")
  }
  lower_weights <- x$internal$experts$lower$coefficients
  upper_weights <- x$internal$experts$upper$coefficients
  gamma         <- x$internal$gamma_grid

  if(type == "last") {
    plot(
      1:length(gamma),
      upper_weights,
      type = "p",
      ylim = range(c(lower_weights, upper_weights)) * c(0.8, 1.2), # Stretch range
      col = "blue",
      xaxt = 'n',
      xlab = expression(Learning~Rates~(gamma)),
      ylab = "Weight",
      ...
    )
    axis(1,
      at = 1:length(gamma),
      labels = gamma
    )
    points(
      1:length(gamma),
      lower_weights,
      col = "red"
    )
    graphics::legend(legend, legend = c("Upper", "Lower"), fill = c("blue", "red"))
  }
  else {
    plot(x$internal$experts$lower, type = "plot_weight", dynamic = FALSE, main = "Upper Weights")
    plot(x$internal$experts$lower, type = "plot_weight", dynamic = FALSE, main = "Lower Weights")
  }
}
