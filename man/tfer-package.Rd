\name{tfer-package}
\alias{tfer-package}
\alias{tfer}
\docType{package}
\title{
  Forensic Glass Transfer Probabilities
}
\description{
  Statistical interpretation of forensic glass transfer (Simulation of
  the probability distribution of recovered glass fragments).
  }
\details{
\tabular{ll}{
Package: \tab tfer\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2010-11-07\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
Depends: \tab methods\cr
}

The \code{tfer} package provides functions for simulating the number of
recovered glass fragments given the conditions set by the user on
factors affecting the transfer, persistence and recovery of glass
fragments. A large simulation size will provide precise estimates of
transfer probabilities to be used in the Bayesian interpretation of
forensic glass evidence.

The \code{\link{transfer}} constructor function creates an object of class
\code{transfer} consisting of a list of simulated number of recovered
glass fragments and the input parameters set by the user. This function
is based on the full graphical model in Curran \emph{et al.} (1998). The
user can specify arguments for simulation size, distance, transfer,
persistence and recovery properties.

The \code{\link{values}} function extracts the simulated random variates from a
\code{transfer} object. \code{\link{para}} returns the input parameters and
user-specified arguments as a numeric vector. \code{\link{parameters}} is an
alternative way of displaying the input parameters and arguments. The
initial information specified by the user are concatenated and displayed
as a string. Users may find this more informative than \code{para} as it
displays what each parameter denotes.   

\code{\link{tprob}} returns the transfer probabilities for each
unique value of the simulated random variates. If the user is only
interested in the probabilities of recovering a certain number of
fragments, this can be specified as the second argument of \code{tprob}.

\code{\link{summary}} provides summary statistics of \code{transfer} objects
and returns a list of input parameters, five-number summary and
probabilities of transfer.

The user has three plotting options for producing a graphical view of a
\code{transfer} object. The plot type can be specified as (0 = barplot
of probabilities, 1 = barplot of frequencies or 2 = histogram).
A barplot of probabilities is set as the default. 
}
\author{
  James Curran and TingYu Huang

  Maintainer: TingYu Huang <thua041@aucklanduni.ac.nz>
}
\references{
  Curran, J. M., Hicks, T. N. & Buckleton, J. S. (2000). \emph{Forensic
    interpretation of glass evidence}. Boca Raton, FL: CRC Press.

  Curran, J. M., Triggs, C. M., Buckleton, J. S., Walsh, K. A. J. &
  Hicks, T. N. (January, 1998). Assessing transfer probabilities in a
  Bayesian interpretation of forensic glass evidence. \emph{Science &
  Justice}, \emph{38}(1), 15-21.
}
\keyword{ package }
\examples{
library(tfer)

## create a transfer object using default arguments
y = transfer()

## probability table
probs = tprob(y)

## extract the probabilities of recovering 8 to 15
## glass fragments given the user-specified arguments
tprob(y, 8:15) 

## produce a summary table for a transfer object
summary(y)

## barplot of transfer probabilities (default)
plot(y, ptype = 0)
plot(y)

## barplot of transfer frequencies
plot(y, ptype = 1)

## histogram
plot(y, ptype = 2)
}