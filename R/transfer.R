#' Glass Transfer, Persistence and Recovery Probabilities
#'
#' Construct a transfer object to simulate the number of glass fragments
#' recovered given the conditions set by the user.
#'
#'
#' @param N Simulation size
#' @param d The breaker's distance from the window
#' @param deffect Distance effect. \code{deffect = TRUE} when distance effect
#'   exists. Otherwise \code{deffect = FALSE}.
#' @param lambda The average number of glass fragments transferred to the
#'   breaker's clothing.
#' @param Q Proportion of high persistence fragments.
#' @param l0 Lower bound on the percentage of fragments lost in the first hour
#' @param u0 Upper bound on the percentage of fragments lost in the first hour
#' @param lstar0 Lower bound on the percentage of high persistence fragments
#'   lost in the first hour
#' @param ustar0 Upper bound on the percentage of high persistence fragments
#'   lost in the first hour
#' @param lj Lower bound on the percentage of fragments lost in the j'th hour
#' @param uj Upper bound on the percentage of fragments lost in the j'th hour
#' @param lstarj Lower bound on the percentage of high persistence fragments
#'   lost in the j'th hour
#' @param ustarj Upper bound on the percentage of high persistence fragments
#'   lost in the j'th hour
#' @param lR Lower bound on the percentage of fragments expected to be detected
#'   in the lab
#' @param uR Upper bound on the percentage of fragments expected to be detected
#'   in the lab
#' @param lt Lower bound on time between commission of crime and apprehension of suspect
#' @param ut Upper bound on time between commission of crime and apprehension of suspect
#' @param r Probability r in ti ~ NegBinom(t, r)
#' @param timeDist the distribution for the random amount of time between the commission of
#' the crime and the apprehension of the suspect. There are three choices \code{"negbin"},
#' \code{"cnegbin"}, and \code{"uniform"}. Before talking about these it should be noted that
#' if \code{lt} is equal to \code{ut} - then there is no randomness in this calculation. If
#' \code{lt} does not equal \code{ut}, then the average of these two values is used in the
#' two negative binomial options: \code{"negbin"} and \code{"cnegbin"}. The difference betweeen
#' them is that \code{"cnegbin"} is a constrained negative binomial where the allowable times are
#' constrained to be between \code{lt} and \code{ut}. If \code{"uniform"} is selected, then
#' a uniformly distributed random time between \code{lt} and \code{ut} is used in each iteration.
#' @param loop if \code{TRUE} an element by element version of the simulation is used,
#' if \code{FALSE} then a (mostly) vectorised element version of the simulation is used. 
#' The results from the two methods appear to be almost identical - they won't be the 
#' same even with the same seed because of the way the random variates are generated. I
#' (James) believe the vectorised version is faster and better. There was also a small mistake
#' which has been corrected in that the initial set of persistent fragments was not
#' being 
#' 
#' @return a \code{list} containing:
#' \describe{
#'   \item{results}{The simulated values of recovered glass fragments}
#'   \item{paramList}{Input parameters}
#' }
#' The returned object has S3 class types tfer and transfer for backwards compatibility 
#' 
#' @importFrom stats dnbinom rgamma rnorm rbinom rnbinom rpois runif
#' 
#' @export
#' @author James Curran and TingYu Huang
#' 
#' 
#' @references Curran, J. M., Hicks, T. N. & Buckleton, J. S. (2000).
#'   \emph{Forensic interpretation of glass evidence}. Boca Raton, FL: CRC
#'   Press.
#'
#'   Curran, J. M., Triggs, C. M., Buckleton, J. S., Walsh, K. A. J. & Hicks T.
#'   N. (January, 1998). Assessing transfer probabilities in a Bayesian
#'   interpretation of forensic glass evidence. \emph{Science & Justice},
#'   \emph{38}(1), 15-21.
#' @examples
#'
#' library(tfer)
#'
#' ## create a transfer object using default arguments
#' y = transfer()
#'
#' ## probability table
#' probs = tprob(y)
#'
#' ## extract the probabilities of recovering 8 to 15
#' ## glass fragments given the user-specified arguments
#' tprob(y, 8:15)
#'
#' ## produce a summary table for a transfer object
#' summary(y)
#'
#' ## barplot of probabilities (default)
#' plot(y)
#' plot(y)
#'
#' ## barplot of transfer frequencies
#' plot(y, ptype = "f")
#'
#' ## histogram
#' plot(y, ptype = "h")
#' 
transfer = function (N = 10000, d = 0.5, deffect = TRUE, lambda = 120,
                     Q = 0.05, l0 = 0.8, u0 = 0.9, lstar0 = 0.1, ustar0 = 0.15,
                     lj = 0.45, uj = 0.7, lstarj = 0.05, ustarj =0.1, lR = 0.5,
                     uR = 0.7, lt = 1, ut = 2, r = 0.5,
                     timeDist = c("negbin", "cnegbin", "uniform"),
                     loop = FALSE) {
  
  ## TODO - paramater range checks
  
  timeDist = match.arg(timeDist)
  
  
  results = rep(NA,N)
  paramList = list(N = N, 
              d = d, 
              deffect = deffect, 
              lambda = lambda, 
              Q = Q, 
              l0 = l0, u0 = u0, 
              lstar0 = lstar0, ustar0 = ustar0,
              lj = lj, uj = uj,
              lstarj = lstarj, ustarj = ustarj,
              lR = lR, uR = uR, 
              lt = lt,
              ut = ut,
              r = r,
              timeDist = timeDist)
  
  
  if((paramList$timeDist == "cnegbin" || paramList$timeDist == "uniform")
      && paramList$lt != paramList$ut){
    ctimes = seq(floor(paramList$lt), ceiling(paramList$ut), by = 1)
    tbar = 0.5 * (paramList$lt + paramList$ut)
    probs = dnbinom(ctimes, tbar, r)
    probs = probs / (1 - sum(probs))
  }
  
  
  if(loop){ 
    for (i in 1:N) {
      di = rgamma(1, paramList$d)
      lambdai = rnorm(1, paramList$lambda, paramList$lambda / 2)
      ##weight = rnorm(1, 1, 0.5)
      ##lambdai = lambda*weight
      
      if(lt == ut){
        ti = lt
      }else{
        if(timeDist == "negbin"){
          ti = rnbinom(1, 0.5 * (paramList$lt + paramList$ut), paramList$r)
        }else if(timeDist == "cnegbin"){
          ti = sample(ctimes, size = 1, prob = probs)
        }else{
          ti = sample(ctimes, size = 1)
        }
      }
      
      
      x0 = if(paramList$deffect){
        rpois(1, abs(lambdai * exp(1 - (di / paramList$d))))
      }
      else{
        rpois(1, abs(lambdai))
      }
      
      q0 = rbinom(1, x0, paramList$Q)
      xj = x0 - q0 - rbinom(1, x0 - q0, runif(1, paramList$l0, paramList$u0))
      qj = q0 - rbinom(1, q0, runif(1, paramList$lstar0, paramList$ustar0))
      
      if (ti > 1) {
        for (j in 2:ti) {
          qj = qj - rbinom(1, qj, runif(1, paramList$lstarj, paramList$ustarj))
          xj = xj - rbinom(1, xj, runif(1, paramList$lj, paramList$uj))
        }
      }
      yi = xj + qj
      results[i] = yi - rbinom(1, yi, 1 - runif(1, paramList$lR, paramList$uR))
    }
  }else{
    di = rgamma(N, paramList$d)
    lambdai = rnorm(N, paramList$lambda, paramList$lambda * 0.5)
    ##weight = rnorm(1, 1, 0.5)
    ##lambdai = lambda*weight
    
    if(lt == ut){
      ti = rep(paramList$ut, N)
    }else{
      if(timeDist == "negbin"){
        ti = rnbinom(N, (paramList$lt + paramList$ut) * 0.5, paramList$r)
      }else if(timeDist == "cnegbin"){
        ti = sample(ctimes, size = N, prob = probs, replace = TRUE)
      }else{
        ti = sample(ctimes, size = N, replace = TRUE)
      }
    }
    
    
    x0 = if(paramList$deffect){
      rpois(N, abs(lambdai * exp(1 - (di / paramList$d))))
    }
    else{
      rpois(N, abs(lambdai))
    }
    
    q0 = rbinom(N, x0, paramList$Q)
    xj = x0 - q0 - rbinom(N, x0 - q0, runif(N, paramList$l0, paramList$u0))
    qj = q0 - rbinom(N, q0, runif(N, paramList$lstar0, paramList$ustar0))
    
    ## This is sequential
    ## Might be a smarter way
  
    
    idx = which(ti > 1)
    
    for(i in idx){
      for (j in 2:ti[i]) {
        if(qj[i] > 0)
          qj[i] = qj[i] - rbinom(1, qj[i], runif(1, paramList$lstarj, paramList$ustarj))
        if(xj[i] > 0)
          xj[i] = xj[i] - rbinom(1, xj[i], runif(1, paramList$lj, paramList$uj))
      }
    }
    
    yi = xj + qj
    results = yi - rbinom(N, yi, 1 - runif(N, paramList$lR, paramList$uR))
  }
  
  results = list(results = results, paramList = paramList)
  class(results) = c("transfer", "tfer")
  
  return(results)
}
