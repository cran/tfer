#' Forensic Glass Transfer Probabilities
#' 
#' Statistical interpretation of forensic glass transfer (Simulation of the
#' probability distribution of recovered glass fragments).
#' 
#' The \code{tfer} package provides functions for simulating the number of
#' recovered glass fragments given the conditions set by the user on factors
#' affecting the transfer, persistence and recovery of glass fragments. A large
#' simulation size will provide precise estimates of transfer probabilities to
#' be used in the Bayesian interpretation of forensic glass evidence.
#' 
#' The \code{\link{transfer}} constructor function creates an object of class
#' \code{transfer} consisting of a list of simulated number of recovered glass
#' fragments and the input parameters set by the user. This function is based
#' on the full graphical model in Curran \emph{et al.} (1998). The user can
#' specify arguments for simulation size, distance, transfer, persistence and
#' recovery properties.
#' 
#' The \code{\link{values}} function extracts the simulated random variates
#' from a \code{transfer} object. \code{\link{para}} returns the input
#' parameters and user-specified arguments as a numeric vector.
#' \code{\link{parameters}} is an alternative way of displaying the input
#' parameters and arguments. The initial information specified by the user are
#' concatenated and displayed as a string. Users may find this more informative
#' than \code{para} as it displays what each parameter denotes.
#' 
#' \code{\link{tprob}} returns the transfer probabilities for each unique value
#' of the simulated random variates. If the user is only interested in the
#' probabilities of recovering a certain number of fragments, this can be
#' specified as the second argument of \code{tprob}.
#' 
#' \code{\link{summary}} provides summary statistics of \code{transfer} objects
#' and returns a list of input parameters, five-number summary and
#' probabilities of transfer.
#' 
#' The user has three plotting options for producing a graphical view of a
#' \code{transfer} object. The plot type can be specified as (0 = barplot of
#' probabilities, 1 = barplot of frequencies or 2 = histogram). A barplot of
#' probabilities is set as the default.
#' 
#' @name tfer
#' @title Forensic Transfer Probabilities
#' @docType package
#' @author James Curran and TingYu Huang
#' 
#' Maintainer: James Curran <j.curran@auckland.ac.nz>
