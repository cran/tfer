\name{transfer}
\alias{transfer}
\title{
  Glass Transfer, Persistence and Recovery Probabilities
}
\description{
  Simulate the number of glass fragments recovered given the conditions
  set by the user.
}
\usage{
transfer(N = 10000, d = 0.5, deffect = 1, lambda = 120, Q = 0.05, l0 =
0.8, u0 = 0.9, lstar0 = 0.1, ustar0 = 0.15, lj = 0.45, uj = 0.7, lstarj
= 0.05, ustarj = 0.1, lR = 0.5, uR = 0.7, t = 1.5, r = 0.5)
}
\arguments{
  \item{N}{
    Simulation size
  }
  \item{d}{
    The breaker's distance from the window
  }
  \item{deffect}{
    Distance effect. \code{deffect = 1} when distance effect
    exists. Otherwise \code{deffect = 0}.
  }
  \item{lambda}{
    The average number of glass fragments transferred to the breaker's
    clothing.
  }
  \item{Q}{
    Proportion of high persistence fragments.
  }
  \item{l0}{
    Lower bound on the percentage of fragments lost in the first hour
  }
  \item{u0}{
    Upper bound on the percentage of fragments lost in the first hour
  }
  \item{lstar0}{
    Lower bound on the percentage of high persistence fragments lost in
    the first hour
  }
  \item{ustar0}{
    Upper bound on the percentage of high persistence fragments lost in
    the first hour
  }
  \item{lj}{
    Lower bound on the percentage of fragments lost in the j'th hour
  }
  \item{uj}{
    Upper bound on the percentage of fragments lost in the j'th hour
  }
  \item{lstarj}{
    Lower bound on the percentage of high persistence fragments lost in
    the j'th hour
  }
  \item{ustarj}{
    Upper bound on the percentage of high persistence fragments lost in
    the j'th hour
  }
  \item{lR}{
    Lower bound on the percentage of fragments expected to be detected in the lab
  }
  \item{uR}{
    Upper bound on the percentage of fragments expected to be detected in the lab
  }
  \item{t}{
    Time between commission of crime and apprehension of suspect
  }
  \item{r}{
    Probability r in ti ~ NegBinom(t, r)
    }
}
\value{
  \item{Y }{The simulated values of recovered glass fragments}
  \item{para }{Input parameters}
}
\references{
  Curran, J. M., Hicks, T. N. & Buckleton, J. S. (2000). \emph{Forensic
    interpretation of glass evidence}. Boca Raton, FL: CRC Press.
  
  Curran, J. M., Triggs, C. M., Buckleton, J. S., Walsh, K. A. J. &
  Hicks T. N. (January, 1998). Assessing transfer probabilities in a
  Bayesian interpretation of forensic glass evidence. \emph{Science &
    Justice}, \emph{38}(1), 15-21.
}
\author{
  James Curran and TingYu Huang
}
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

## histogram (default)
plot(y, ptype = 0)
plot(y)

## barplot of transfer frequencies
plot(y, ptype = 1)

## barplot of transfer probabilities
plot(y, ptype = 2)
}
