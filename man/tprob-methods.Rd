\name{tprob-methods}
\docType{methods}
\alias{tprob-methods}
\title{ Methods for Function tprob }
\description{
  Methods for function \code{tprob} 
}
\section{Methods}{
  \describe{
    
    \item{\code{signature(object = "transfer", x = "missing")}}{
      If the second argument is not specified, \code{tprob} returns the
      transfer probabilities for each unique value of simulated
      recovered glass fragments.
    }
    
    \item{\code{signature(object = "transfer", x = "numeric")}}{
      \code{tprob} returns the probabilities of recovering the specified
      \emph{x} number of glass fragments.
      }
}}
\keyword{methods}


