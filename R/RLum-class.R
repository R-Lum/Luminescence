#' Class \code{"RLum"}
#'
#' Abstract class for data in the package Luminescence
#'
#'
#' @name RLum-class
#'
#' @docType class
#'
#' @slot originator Object of class "character" containing the name of the producing function for the object
#'
#' @slot .uid Object of class "character" containing a unified object identifier
#'
#' @note \code{RLum} is a virtual class.
#'
#' @section Objects from the Class: A virtual Class: No objects can be created
#' from it.
#'
#' @section Class version: 0.3.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @seealso \code{\linkS4class{RLum.Data}}, \code{\linkS4class{RLum.Analysis}}
#'
#' @keywords classes
#'
#' @examples
#'
#' showClass("RLum")
#' 
#' @export
setClass("RLum",
           slots = list(
             originator = "character",
             .uid = "character"),
           contains = "VIRTUAL",
           prototype = prototype(
             originator = NA_character_,
             .uid = NA_character_
           ),
         sealed = TRUE)

