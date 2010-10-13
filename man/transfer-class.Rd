\name{transfer-class}
\Rdversion{1.1}
\docType{class}
\alias{transfer-class}
\alias{[,transfer,ANY,missing,missing-method}
\alias{Compare,transfer,numeric-method}
\alias{parameters,transfer-method}
\alias{plot,transfer-method}
\alias{show,transfer-method}
\alias{summary,transfer-method}
\alias{tprob,transfer,missing-method}
\alias{tprob,transfer,numeric-method}

\title{Class "transfer"}
\description{
  An S4 class.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("transfer", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{para}:}{Object of class \code{"numeric"} }
    \item{\code{Y}:}{Object of class \code{"numeric"} }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "transfer", i = "ANY", j = "missing", drop = "missing")}: ... }
    \item{Compare}{\code{signature(e1 = "transfer", e2 = "numeric")}: ... }
    \item{parameters}{\code{signature(object = "transfer")}: ... }
    \item{plot}{\code{signature(x = "transfer")}: ... }
    \item{show}{\code{signature(object = "transfer")}: ... }    
    \item{summary}{\code{signature(object = "transfer")}: ... }  
    \item{tprob}{\code{signature(object = "transfer", x = "missing")}: ... }    
    \item{tprob}{\code{signature(object = "transfer", x = "numeric")}: ... }
 }
}
\author{
TingYu Huang
}
\examples{
showClass("transfer")
}
\keyword{classes}
