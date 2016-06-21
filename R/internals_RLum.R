####################################################################################################
##                     INTERNAL HELPER FUNCTIONS                                                  ##
####################################################################################################

#+++++++++++++++++++++
#+ .set_pid()        +
#+++++++++++++++++++++

#' Set unique id of the RLum.Analysis object as parent id for each RLum.Data object in the record list
#'
#' This function only applies on RLum.Analysis objects and was written for performance not
#' usability, means the functions runs without any checks and is for internal usage only.
#'
#' @param \code{\linkS4class{RLum.Analysis}} (\bold{required}): input object where the function
#' should be applied on
#'
#' @return
#' Returns the same object as the input
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @examples
#'
#' ##example using self created data
#' object <- set_RLum(
#' "RLum.Analysis",
#' records = list(
#'  set_RLum("RLum.Data.Curve"),
#'  set_RLum("RLum.Data.Curve")))
#'
#' object <- .set_pid(object)
#'
#' @noRd
.set_pid <- function(object){

  object@records <-
    lapply(object@records, function(x) {
      x@.pid  <- object@.uid
      return(x)
    })

  return(object)
}

#+++++++++++++++++++++
#+ .warningCatcher()        +
#+++++++++++++++++++++

#' Catches warning returned by a function and merges them.
#' The original return of the function is returned. This function is in particular
#' helpful if a function returns a lot of warnings with the same content.
#'
#' @param expr \code{\link{expression}} (\bold{required}): the R expression, usually a
#' function
#'
#' @return
#' Returns the same object as the input and a warning table
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @examples
#'
#' f <- function() {
#'  warning("warning 1")
#'  warning("warning 1")
#'  warning("warnigs 2")
#'  1:10
#' }
#' print(.warningCatcher(f()))
#'
#' @noRd
.warningCatcher <- function(expr) {
  ##set variables
  warning_collector <- list()
  env <-  environment()

  ##run function and catch warnings
  results <- withCallingHandlers(
    expr = expr,
    warning = function(c) {
      assign(x = "warning_collector",
             value = c,
             envir = env)
      invokeRestart("muffleWarning")
    }
  )

  ##set new warning messages with merged results
  if (length(warning_collector) > 0) {
    w_table <- table(as.character(unlist(warning_collector)))
    w_table_names <- names(w_table)

    for (w in 1:length(w_table)) {
      warning(paste(
        w_table_names[w],
        "This warning occurred",
        w_table[w],
        "times!"
      ),
      call. = FALSE)

    }

  }
  return(results)

}
