#' Plot an ACI object.
#'
#' @param x object of class "aci"
#' @param index indexes of the observation and intervals to plot (defaults to showing all observations and intervals).
#' @param legend logical indicating whether or not to include a legend
#' @param ... additional arguments to base plot function
#'
#' @importFrom graphics plot lines legend
#' @export
plot.aci <- function(x, index = NULL, legend = TRUE, predictions = TRUE, ...) {
  params <- list(...)

  if(is.null(index)) index = 1:length(x$Y)

  ylim <- range(c(x$Y[index], x$intervals[index, 1], x$intervals[index, 2]), na.rm = TRUE)
  col <- ifelse(is.na(x$covered[index]), "black", ifelse(x$covered[index], "darkblue", "darkred"))
  plot(
    x$Y[index],
    ylim = ylim,
    col = col,
    xlab = "Index",
    ylab = "Y",
    ...
  )
  lines(x$intervals[index, 1])
  lines(x$intervals[index, 2])

  legend_text <- c("Within interval", "Outside interval")
  legend_col <- c("darkblue", "darkred")
  legend_pch = c(1, 1)

  if(predictions == TRUE) {
    legend_text <- c(legend_text, "Predictions")
    legend_col <- c(legend_col, "green")
    legend_pch <- c(legend_pch, 45)
    for(index in 1:ncol(x$predictions)) {
      lines(x$predictions[, index], col = "green", lty = 1, lwd = 1)
    }
  }


  if(legend == TRUE) {
    legend("topright", legend = legend_text, col = legend_col, pch = legend_pch)
  }
}
