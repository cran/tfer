\name{para}
\alias{para}
\title{
  Extract Transfer and Persistence Parameters I
}
\description{
  Displays input parameters and arguments passed to \code{\link{transfer}}.
}
\usage{
para(object)
}
\arguments{
  \item{object }{An object of class \code{transfer}
}
}
\details{
  \code{para} is one of the two accessor functions for a \code{transfer} object.
}
\value{
  \code{para} returns a vector of input parameters and their
  corresponding arguments.
}
\author{
TingYu Huang
}
\seealso{
\code{\link{parameters}}, \code{\link{transfer}}
}
\examples{
library(tfer)

y = transfer()

para(y)
}