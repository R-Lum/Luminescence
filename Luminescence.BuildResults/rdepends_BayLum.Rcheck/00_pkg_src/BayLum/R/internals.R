## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Internal Helper Functions for BayLum
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#'@title Create BayLum List
#'
#'@description This function works similar to [list], except the case that it sets a proper
#'class and originator
#'
#'@param ... arguments passed to [list]
#'
#'@param originator [character] (*with default*): argument to set originator manually
#'
#'@author Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS-Université Bordeaux Montaigne (France)
#'
#'@md
#'@noRd
.list_BayLum <- function(..., originator = sys.call(which = -1)[[1]]){
  ## set list
  l <- list(...)

  ## update originators
  attr(l, "class") <- "BayLum.list"
  attr(l, "originator") <- as.character(originator)

  return(l)

}

#' Bayesian Credible Interval
#'
#' Computes the shortest credible interval of the output of the MCMC algorithm
#' for a single parameter
#' @param a_chain Numeric vector containing the output of the MCMC algorithm
#'  for the parameter.
#' @param level Probability corresponding to the level of confidence used for
#'  the credible interval, default = 0.95.
#' @param roundingOfValue Integer indicating the number of decimal places to be
#'  used, default = 0.
#' @details
#'  A \eqn{(100 * level)}\% credible interval is an interval that keeps \eqn{N * (1 - level)}
#'  elements of the sample outside the interval.
#'
#'  The \eqn{(100 * level)}\% credible interval is the shortest of all those intervals.
#' @return
#'  A named vector of values containing the confidence level and the endpoints
#'  of the shortest credible interval in calendar years (BC/AD).
#' @examples
#'   data(Events); attach(Events)
#'   CredibleInterval(Event.1)
#'   CredibleInterval(Event.12, 0.50)
#' @author A. Philippe, M.-A. Vibet
#' @noRd
CredibleInterval <- function(a_chain, level = 0.95, roundingOfValue = 0) {
  sorted_sample <- sort(a_chain) # Ordering the sample
  N <- length(a_chain)           # Calculation of the sample size
  OutSample <- N * (1 - level)   # Calculation of the number of data to be outside the interval

  # Combinasion of all credible intervals
  I = cbind(sorted_sample[1:(OutSample + 1)],
            sorted_sample[(N - OutSample):N])

  l = I[, 2] - I[, 1]   # Length of intervals
  i <- which.min(l)     # Look for the shortest interval

  # Returns the level and the endpoints rounded
  return(
    c(
      "level" = level,
      "Credible.Interval.Inf" = round(I[i, 1], digits = roundingOfValue),
      "Credible.Interval.Sup" = round(I[i, 2], digits = roundingOfValue)
    )
  )
}
