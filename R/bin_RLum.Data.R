#' Channel binning - method dispatchter
#'
#' Function calls the object-specific bin functions for RLum.Data S4 class objects.
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
#' @return An object of the same type as the input object is provided
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#' (France)
#'
#' @note Currenlty only \code{RLum.Data} objects of class \code{RLum.Data.Curve} are supported!
#'
#' @seealso
#' \code{\linkS4class{RLum.Data.Curve}}
#'
#' @examples
#'
#' ##load example data
#' data(ExampleData.CW_OSL_Curve, envir = environment())
#'
#' ##create RLum.Data.Curve object from this example
#' curve <-
#'   set_RLum(
#'       class = "RLum.Data.Curve",
#'       recordType = "OSL",
#'       data = as.matrix(ExampleData.CW_OSL_Curve)
#'   )
#'
#' ##plot data without and with 2 and 4 channel binning
#' plot_RLum(curve)
#' plot_RLum(bin_RLum.Data(curve, bin_size = 2))
#' plot_RLum(bin_RLum.Data(curve, bin_size = 4))
#'
#' @keywords utilities
#'
#' @export
setGeneric("bin_RLum.Data", function(object, ...) {
  standardGeneric("bin_RLum.Data")
})
