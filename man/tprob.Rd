\name{tprob}
\alias{tprob}
\title{
  Transfer Probabilities
}
\description{
  \code{tprob} is a generic function which returns the probability
  distribution of recovered glass fragments.
}
\usage{
tprob(object, x)
}
\arguments{
  \item{object}{A \code{transfer} object.}
  \item{x}{A \code{numeric} object.}
}
\value{
  Returns a \code{numeric} vector of glass transfer probabilities given
  the initial conditions set by the user.
}
\author{
  TingYu Huang
}
\seealso{
\code{\link{transfer}}
}
\examples{
library(tfer)

y = transfer()

#Full probability table
tprob(y)

#Probability of recovering 8 glass fragments
tprob(y, 8)
}

