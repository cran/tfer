\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,ANY-method}
\title{ Methods for Function plot }
\description{
  Methods for function \code{plot}
}
\section{Methods}{
\describe{
  \item{\code{signature(x = "ANY")}}{
    \code{plot} methods for any \R objects.
  }
  
  \item{\code{signature(x = "transfer")}}{
    \code{plot} method for \code{transfer} objects. Allows histograms,
    barplots of frequencies and probabilities of recovered glass
    fragments to be plotted. The plot type can be specified as (0 =
    histogram, 1 = barplot of frequencies or 2 = barplot of
    probabilities). Histogram is set as the default plot.
}}}
\keyword{methods}
\keyword{plot}
