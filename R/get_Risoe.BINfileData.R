#' General accessor function for RLum S4 class objects
#'
#' Function calls object-specific get functions for RisoeBINfileData S4 class objects.
#'
#' The function provides a generalised access point for specific
#' [Risoe.BINfileData-class] objects. \cr
#' Depending on the input object, the corresponding get function will be selected.
#' Allowed arguments can be found in the documentations of the corresponding 
#' [Risoe.BINfileData-class] class.
#'
#' @param object [Risoe.BINfileData-class] (**required**): 
#' S4 object of class `RLum`
#' 
#' @param ... further arguments that one might want to pass to the specific
#' get function
#' 
#' @return Return is the same as input objects as provided in the list
#' 
#' @section Function version: 0.1.0
#' 
#' @author 
#' Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#' 
#' @seealso [Risoe.BINfileData-class]
#' 
#' @keywords utilities
#'
#' @md
#' @export
setGeneric(
  name = "get_Risoe.BINfileData",
  def = function(object, ...) {
    standardGeneric("get_Risoe.BINfileData")
  },
  package = "Luminescence"
)
