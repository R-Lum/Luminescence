#' General accessor function for RLum.Data S4 class objects
#'
#' Function calls object-specific bin functions for RLum.Data S4 class objects.
#'
#' The function provides a generalised access point for specific
#' \code{\linkS4class{RLum.Data}} objects.\cr Depending on the input object, the
#' corresponding function will be selected. Allowed arguments can be found
#' in the documentations of the corresponding \code{\linkS4class{RLum.Data}} class.
#'
#' @param object \code{\linkS4class{RLum.Data}} (\bold{required}): S4 object of
#' class \code{RLum.Data}
#'
#' @param ... further arguments passed to the specifc class method
#'
#' @return Return is the same as input
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#' (France)
#'
#' @seealso
#' \code{\linkS4class{RLum.Data}}
#'
#' @keywords utilities
#'
#' @export
setGeneric("bin_RLum.Data", function(object, ...) {
  standardGeneric("bin_RLum.Data")
})
