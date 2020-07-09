#' plot method for objects of transfer class
#'
#' @param x an object of class \code{transfer}
#' @param ptype, one of \code{"density"}, \code{"freq"}, or \code{"hist"}. \code{"density"} will give a barplot with probability
#' on the y-axis, \code{"frequency"} will give a barplot with frequencies (counts) on the y-axis, and \code{"hist"} will
#' produce a historgram with frequency (counts) on the y-axis. One-letter versions will also work, i.e. \code{"d"}, \code{"f"} and
#' \code{"h"}. The original \code{0}, \code{1}, \code{2} will also work, but this usage is deprecated and will produce a warning.
#' @param xlab the x-axis label, by default "n"
#' @param main the plot title, empty by default
#' @param col the colour of the bars in the plot, by default "red"
#' @param \dots any other arguments to be passed to \code{barplot} or \code{histogram}
#'
#' @importFrom graphics axis barplot box hist par
#' @export
plot.tfer = function(x,
                     ptype = c("density", "freq", "hist"),
                     xlab = "n",
                     main = "",
                     col = "red",
                     ...) {
  if (is.numeric(ptype)) {
    ptype = switch(ptype + 1, "d", "f", "h")
    warning(
      "This usage is deprecated. Please use ptype = \"density\", ptype = \"freq\", or ptype = \"hist\" in the future"
    )
  }
  
  ptype = match.arg(ptype)
  
  if (ptype == "density") {
    probs = tprob(x)
    nmax = length(probs) - 1
    
    b = barplot(
      probs,
      xlab = xlab,
      ylab = "Probability",
      main = main,
      col = col,
      names.arg = FALSE,
      xaxs = "i",
      yaxs = "i",
      ylim = c(0, max(probs) * 1.05),
      axes = FALSE,
      ...
    )
    axis(2)
    axis(
      1,
      at = c(b[1], b[nmax + 1]),
      tick = FALSE,
      labels = c(0, nmax)
    )
    box()
  } else if (ptype == "freq") {
    v = getValues(x)
    m = max(v)
    counts = tabulate(v + 1, m + 1)
    nmax = length(counts) - 1
    
    b = barplot(
      counts,
      xlab = xlab,
      ylab = "Frequency",
      main = main,
      col = col,
      names.arg = FALSE,
      xaxs = "i",
      yaxs = "i",
      ylim = c(0, ceiling(max(counts) * 1.05)),
      axes = FALSE, 
      ...
    )
    axis(2)
    axis(
      1,
      at = c(b[1], b[nmax + 1]),
      tick = FALSE,
      labels = c(0, nmax)
    )
    box()
  } else {
    hist(getValues(x),
         xlab = xlab,
         col = col,
         main = main,
         ...)
  }
}
