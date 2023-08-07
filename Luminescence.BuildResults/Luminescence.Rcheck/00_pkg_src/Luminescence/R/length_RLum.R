#' General accessor function for RLum S4 class objects
#'
#' Function calls object-specific get functions for RLum S4 class objects.
#'
#' The function provides a generalised access point for specific
#' [RLum-class] objects.\cr
#' Depending on the input object, the corresponding get function will be selected.
#' Allowed arguments can be found in the documentations of the corresponding
#' [RLum-class] class.
#'
#' @param object [RLum-class] (**required**):
#' S4 object of class `RLum`
#'
#' @return Return is the same as input objects as provided in the list.
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#' (France)
#' @seealso
#' [RLum.Data.Curve-class],
#' [RLum.Data.Image-class],
#' [RLum.Data.Spectrum-class],
#' [RLum.Analysis-class],
#' [RLum.Results-class]
#' @keywords utilities
#'
#'
#' @md
#' @export
setGeneric("length_RLum", function(object) {
  standardGeneric("length_RLum")
})

