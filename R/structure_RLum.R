#' General structure function for RLum S4 class objects
#'
#' Function calls object-specific get functions for RLum S4 class objects.
#'
#' The function provides a generalised access point for specific
#' \code{\linkS4class{RLum}} objects.\cr Depending on the input object, the
#' corresponding structure function will be selected. Allowed arguments can be found
#' in the documentations of the corresponding \code{\linkS4class{RLum}} class.
#'
#' @param object \code{\linkS4class{RLum}} (\bold{required}): S4 object of
#' class \code{RLum}
#'
#' @param \dots further arguments that one might want to pass to the specific
#' structure method
#'
#' @return Returns a \code{data.frame} with structure of the object.
#'
#' @section Function version: 0.2.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#' (France)
#'
#' @seealso
#' \code{\linkS4class{RLum.Data.Curve}},
#' \code{\linkS4class{RLum.Data.Image}},
#' \code{\linkS4class{RLum.Data.Spectrum}},
#' \code{\linkS4class{RLum.Analysis}},
#' \code{\linkS4class{RLum.Results}}
#'
#' @keywords utilities
#'
#' @examples
#'
#' ##load example data
#' data(ExampleData.XSYG, envir = environment())
#'
#' ##show structure
#' structure_RLum(OSL.SARMeasurement$Sequence.Object)
#'
#' @export
setGeneric("structure_RLum", function(object, ...) {
  standardGeneric("structure_RLum")
})
