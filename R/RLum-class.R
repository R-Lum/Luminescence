#' Class \code{"RLum"}
#'
#' Abstract class for data in the package Luminescence
#'
#'
#' @name RLum-class
#'
#' @docType class
#'
#' @note \code{RLum} is a virtual class.
#'
#' @section Objects from the Class: A virtual Class: No objects can be created
#' from it.
#'
#' @section Class version: 0.2.0
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
setClass("RLum",
         contains = "VIRTUAL")
