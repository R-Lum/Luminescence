#' Class \code{"RLum.Data"}
#'
#' Generalized virtual data class for luminescence data.
#'
#'
#' @name RLum.Data-class
#'
#' @docType class
#'
#' @note Just a virtual class.
#'
#' @section Objects from the Class: A virtual Class: No objects can be created
#' from it.
#'
#' @section Class version: 0.2.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @seealso \code{\linkS4class{RLum}}, \code{\linkS4class{RLum.Data.Curve}},
#' \code{\linkS4class{RLum.Data.Spectrum}}
#'
#' @keywords classes
#'
#' @examples
#'
#' showClass("RLum.Data")
#'
setClass("RLum.Data",
         contains = "RLum"
         )
