\name{parameters}
\alias{parameters}
\title{
  Extract Transfer and Persistence Parameters II
}
\description{
   The initial information specified by the user are concatenated and
   displayed as a string. Users may find this more informative than
   \code{\link{para}} as it displays what the parameters denote. 
}
\usage{
parameters(object)
}
\arguments{
  \item{object}{An object of class \code{transfer}}
}
\details{
  \code{parameters} is a generic function with a default method only.
}
\value{
  \code{parameters} displays the input parameters and arguments provided
  by the user as a string.
  }
\author{
  TingYu Huang
}
\seealso{
  \code{\link{para}}, \code{\link{transfer}}
}
\examples{
library(tfer)
y = transfer()

parameters(y)

## compare with para(y)
para(y)
}

