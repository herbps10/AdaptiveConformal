#' @export
plot.aci <- function(object, index = NULL, legend = TRUE, ...) {
  if(is.null(index)) index = 1:length(object$Y)
  ylim <- range(c(object$Y[index], object$intervals[index, 1], object$intervals[index, 2]))
  col <- ifelse(is.na(object$covered[index]), "black", ifelse(object$covered[index], "darkblue", "darkred"))
  plot(
    object$Y[index],
    ylim = ylim,
    col = col,
    xlab = "Index",
    ylab = "Y",
    ...
  )
  lines(object$intervals[index, 1])
  lines(object$intervals[index, 2])
  if(legend == TRUE) {
    legend("topright", legend = c("Within interval", "Outside interval"), fill = c("darkblue", "darkred"))
  }
}
