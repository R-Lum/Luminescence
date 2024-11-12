#' General accessor function for RLum S4 class objects
#'
#' Function calls object-specific get functions for RisoeBINfileData S4 class objects.
#'
#' The function provides a generalised access point for specific
#' [Risoe.BINfileData-class] objects.\cr 
#' Depending on the input object, the corresponding get function will be selected. 
#' Allowed arguments can be found in the documentations of the corresponding 
#' [Risoe.BINfileData-class] class.
#'
#' @param METADATA x
#' 
#' @param DATA x
#' 
#' @param .RESERVED x
#'
#' @return Return is the same as input objects as provided in the list.
#' 
#' @section Function version: 0.1
#' 
#' @author 
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#' 
#' @seealso [Risoe.BINfileData-class]
#' 
#' @keywords utilities
#'
#' @md
#' @export
setGeneric("set_Risoe.BINfileData",
  function(METADATA = data.frame(), DATA = list(), .RESERVED = list()) {
    standardGeneric("set_Risoe.BINfileData")
  },
  package = "Luminescence"
)
