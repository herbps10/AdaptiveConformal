#' Plot an ACI object.
#'
#' @param x object of class "aci"
#' @param index indexes of the observation and intervals to plot (defaults to showing all observations and intervals).
#' @param legend logical indicating whether or not to include a legend
#' @param ... additional arguments to base plot function
#'
#' @importFrom graphics plot lines legend
#' @export
plot.aci <- function(x, index = NULL, legend = TRUE, ...) {
  params <- list(...)

  if(is.null(index)) index = 1:length(x$Y)

  ylim <- range(c(x$Y[index], x$intervals[index, 1], x$intervals[index, 2]))
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
  if(legend == TRUE) {
    legend("topright", legend = c("Within interval", "Outside interval"), fill = c("darkblue", "darkred"))
  }
}
