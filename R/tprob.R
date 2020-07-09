#' Return a table of T probabilities for all observed values
#'
#' @param tferObj an object of class \code{transfer} 
#' @param x an optional set of values which specify the desired T-terms. 
#' E.g. x = c(0,1,2) would return T0, T1, and T2 and so on. Negative values of x will 
#' cause the function to stop. Values of x which exceed those observed will be assigned 
#' a value of zero. The return values will be returned in ascending order regardless of 
#' the order of x (although I suppose I could preserve the order if someone really cares).
#'
#' @return A table of T probabilities, giving the probability that x fragments 
#' were recovered given they were transferred and persisted according to the 
#' other inputs of the model. 
#'
#' @examples 
#' set.seed(123)
#' y = transfer()
#' 
#' tprob(y)
#' tprob(y, 55:120) ## max observed value is 113
#' 
#' @export
tprob = function(tferObj, x) {
  v = getValues(tferObj)
  m = max(v)
  probs = tabulate(v + 1, m + 1) / tferObj$paramList$N
  names(probs) = 0:m
  
  if (missing(x))
    return(probs)
  ##else
  
  x = sort(x) ## sort x into ascending order so that we don't have to
  ## put zeros (for x > m) in any other place except the end of the vector
  if (any(x < 0)) {
    stop("x cannot contain negative values")
  }
  
  if (any(x > m)) {
    excess = sum(x > m)
    x = x[x <= m]
    probs = c(probs[x + 1], rep(0, excess))
    names(probs) = c(x, (m + 1):(m + excess))
  } else{
    probs = probs[x + 1]
    names(probs) = x
  }
  
  return(probs)
}
