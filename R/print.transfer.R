#' \code{print} method for transfer objects
#' 
#'  Prints a summary of the simulation input parameters
#' 
#' @param x an object of class transfer
#' @param \dots included for consistency but not used
#' 
#' @keywords methods print
#' @export
print.transfer = function(x, ...){
  paramList = x$paramList
  
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
}