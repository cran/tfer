#' \code{summary} method for transfer objects
#' 
#'  Prints a summary of the simulation input parameters
#' 
#' @param object an object of class transfer
#' @param \dots extra arguments passed to \code{\link{summary.default}}

#' 
#' @return A \code{list} with three elements is returned invisibly: 
#' \describe{
#'   \item{parameters}{list containing all the simulation parameters}
#'   \item{values}{a numeric vector of the simulated values}
#'   \item{probability}{a named numeric vector giving the probability of recovering 0, 1, 2, \ldots fragments}
#' }
#' 
#' @keywords methods summary
#' @export
summary.transfer = function(object, ...){
  paramList = object$paramList
  
  cat("\n", "Simulation N = ", paramList$N,"\n",
      "Distance = ", paramList$d,"\n",
      "Average transferred = ", paramList$lambda, "\n",
      "% High persistence = ", paramList$Q, "\n",
      paramList$l0, "<= % loss in first hour <=", paramList$u0,"\n",
      paramList$lstar0, "<= % high persistence loss in first hour <=", paramList$ustar0,"\n",
      paramList$lj, "<= % loss in j'th hour <=", paramList$uj,"\n",
      paramList$lstarj, "<= % high persistence loss in j'th hour <=", paramList$ustarj,"\n",
      paramList$lR, "<= % detected in lab <=", paramList$uR,"\n",
      "Time = ", paramList$t,"\n", "\n")
  
  invisible(list(parameters = object$paramList, values = summary(object$results, ...),
                          probability = tprob(object)))
}



