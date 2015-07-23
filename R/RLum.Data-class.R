#' Class \code{"RLum.Data"}
#'
#' Generalized virtual data class for luminescence data.
#'
#'
#' @name RLum.Data-class
#' 
#' @docType class
#' 
#' @slot .S3Class Object of class "character"
#' 
#' @note Just a virtual class.
#' 
#' @section Objects from the Class: A virtual Class: No objects can be created
#' from it.
#' 
#' @author Sebastian Kreutzer, 2013 (Freiberg Instruments/JLU Giessen, Germany)
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
         representation(),
         contains = "RLum",
         S3methods=TRUE
         )
