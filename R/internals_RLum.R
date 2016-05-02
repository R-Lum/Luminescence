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
