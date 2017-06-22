#' S4-names function for RLum S4 class objects
#'
#' Function calls object-specific names functions for RLum S4 class objects.
#'
#' The function provides a generalised access point for specific
#' [RLum-class] objects.\cr Depending on the input object, the
#' corresponding 'names' function will be selected. Allowed arguments can be found
#' in the documentations of the corresponding [RLum-class] class.
#'
#' @param object [RLum-class] (\bold{required}): S4 object of
#' class \code{RLum}
#' @return Returns a [character]
#' @section Function version: 0.1.0
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#' (France)
#' @seealso
#' [RLum.Data.Curve-class],
#' [RLum.Data.Image-class],
#' [RLum.Data.Spectrum-class],
#' [RLum.Analysis-class],
#' [RLum.Results-class]
#' @keywords utilities
#' @aliases names_RLum
#'
#' @md
#' @export
setGeneric("names_RLum", function(object) {
  standardGeneric("names_RLum")
})
