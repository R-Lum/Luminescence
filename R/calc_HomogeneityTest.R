#' @title Apply a simple homogeneity test after Galbraith (2003)
#'
#' @description
#' A simple homogeneity test for De estimates.
#' For details see Galbraith (2003).
#'
#' @param data [RLum.Results-class] or [data.frame] (**required**):
#' for [data.frame]: two columns with De `(data[,1])` and De error `(values[,2])`
#'
#' @param log [logical] (*with default*):
#' perform the homogeneity test with (un-)logged data
#'
#' @param ... further arguments (`verbose`).
#'
#' @return
#' Returns a terminal output. In addition an
#' [RLum.Results-class]-object is returned containing the
#' following elements:
#'
#' \item{summary}{[data.frame] summary of all relevant model results.}
#' \item{data}{[data.frame] original input data}
#' \item{args}{[list] used arguments}
#' \item{call}{[call] the function call}
#'
#' The output should be accessed using the function [get_RLum].
#'
#' @section Function version: 0.3.0
#'
#' @author Christoph Burow, University of Cologne (Germany), Sebastian Kreutzer,
#' IRAMAT-CRP2A, Université Bordeaux Montaigne (France)
#'
#' @seealso [stats::pchisq]
#'
#' @references
#' Galbraith, R.F., 2003. A simple homogeneity test for estimates
#' of dose obtained using OSL. Ancient TL 21, 75-77.
#'
#' @examples
#'
#' ## load example data
#' data(ExampleData.DeValues, envir = environment())
#'
#' ## apply the homogeneity test
#' calc_HomogeneityTest(ExampleData.DeValues$BT998)
#'
#' ## using the data presented by Galbraith (2003)
#' df <-
#'  data.frame(
#'    x = c(30.1, 53.8, 54.3, 29.0, 47.6, 44.2, 43.1),
#'    y = c(4.8, 7.1, 6.8, 4.3, 5.2, 5.9, 3.0))
#'
#' calc_HomogeneityTest(df)
#'
#'
#' @md
#' @export
calc_HomogeneityTest <- function(
  data,
  log = TRUE,
  ...
) {
  .set_function_name("calc_HomogeneityTest")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(data, c("data.frame", "RLum.Results"))
  .validate_not_empty(data)
  if (inherits(data, "RLum.Results")) {
    data <- get_RLum(data, "data")
  }
  .validate_logical_scalar(log)

  ##==========================================================================##
  ## ... ARGUMENTS
  ##==========================================================================##
  extraArgs <- list(...)

  ## set plot main title
  if("verbose" %in% names(extraArgs)) {
    verbose<- extraArgs$verbose
  } else {
    verbose<- TRUE
  }

  ##============================================================================##
  ## CALCULATIONS
  ##============================================================================##
  if(log) {
    dat <- log(data)
    dat[[2]] <- data[[2]]/data[[1]]

  } else {
    dat <- data
  }

  wi <- 1 / dat[[2]] ^ 2
  wizi <- wi * dat[[1]]
  mu <- sum(wizi) / sum(wi)
  gi <- wi * (dat[[1]] - mu) ^ 2

  G <- sum(gi)
  df <- length(wi) - 1
  n <- length(wi)

  P <- stats::pchisq(G, df, lower.tail = FALSE)


  ##============================================================================##
  ## OUTPUT
  ##============================================================================##

  if(verbose) {
    cat("\n [calc_HomogeneityTest()]")
    cat("\n\n ---------------------------------")
    cat("\n n:                 ", n)
    cat("\n ---------------------------------")
    cat("\n mu:                ", round(mu,4))
    cat("\n G-value:           ", round(G,4))
    cat("\n Degrees of freedom:", df)
    cat("\n P-value:           ", round(P,4))
    cat("\n ---------------------------------\n\n")
  }

  ##============================================================================##
  ## RETURN VALUES
  ##============================================================================##
  summary <- data.frame(
    n = n,
    g.value = G,
    df = df,
    P.value = P
  )

  args <- list(log = log)

  return(set_RLum(
    class = "RLum.Results",
    data = list(
      summary = summary,
      data = data,
      args = args
    ),
    info = list(call = sys.call())
  ))
}
