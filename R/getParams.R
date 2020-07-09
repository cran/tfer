#' Extract Transfer and Persistence Parameters I
#' 
#' Displays input parameters and arguments passed to \code{\link{transfer}}.
#' 
#' \code{getParams} is one of the two accessor functions for a \code{transfer}
#' object.
#' 
#' @param tferObj An object of class \code{transfer}
#' @return \code{getParams} returns a list of input parameters and their corresponding values.
#' @author TingYu Huang
#' @export
#' 
#' @seealso \code{\link{transfer}}
#' @examples
#' 
#' library(tfer)
#' 
#' y = transfer()
#' 
#' getParams(y)
#' 
getParams = function(tferObj) 
  tferObj$paramList
