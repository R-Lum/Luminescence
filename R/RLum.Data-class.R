#' @title Class `"RLum.Data"`
#'
#' @description Generalized virtual data class for luminescence data.
#'
#'
#' @name RLum.Data-class
#'
#' @docType class
#'
#' @note Just a virtual class.
#'
#' @section Objects from the Class:
#' A virtual Class: No objects can be created from it.
#'
#' @section Class version: 0.2.1
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum-class], [RLum.Data.Curve-class],
#' [RLum.Data.Spectrum-class], [RLum.Data.Image-class]
#'
#' @keywords classes internal
#'
#' @examples
#'
#' showClass("RLum.Data")
#'
#' @md
#' @export
setClass("RLum.Data",
         contains = c("RLum", "VIRTUAL")
)

