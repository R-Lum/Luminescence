#' General structure function for RLum S4 class objects
#'
#' Function calls object-specific get functions for RLum S4 class objects.
#'
#' The function provides a generalised access point for specific
#' [RLum-class] objects.\cr Depending on the input object, the
#' corresponding structure function will be selected. Allowed arguments can be found
#' in the documentations of the corresponding [RLum-class] class.
#'
#' @param object [RLum-class] (\bold{required}): S4 object of
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
#' [RLum.Data.Curve-class],
#' [RLum.Data.Image-class],
#' [RLum.Data.Spectrum-class],
#' [RLum.Analysis-class],
#' [RLum.Results-class]
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
#' @md
#' @export
setGeneric("structure_RLum", function(object, ...) {
  standardGeneric("structure_RLum")
})
