#' Class \code{"TLum.Data"}
#'
#' Generalized virtual data class for luminescence data.
#'
#'
#' @name TLum.Data-class
#' @rdname TLum.Data-class
#'
#' @docType class
#'
#' @author David Strebler
#'
#' @keywords classes
#'
#' @exportClass TLum.Data


setClass("TLum.Data",
         representation(),
         contains = "TLum"
         )
