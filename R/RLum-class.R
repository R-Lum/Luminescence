#' Class `"RLum"`
#'
#' Abstract class for data in the package Luminescence
#' Subclasses are:
#'
#' **RLum-class**\cr
#' |\cr
#' |----[RLum.Data-class]\cr
#' |----|-- [RLum.Data.Curve-class]\cr
#' |----|-- [RLum.Data.Spectrum-class]\cr
#' |----|-- [RLum.Data.Image-class]\cr
#' |----[RLum.Analysis-class]\cr
#' |----[RLum.Results-class]
#'
#' @name RLum-class
#'
#' @docType class
#'
#' @slot originator
#' Object of class [character] containing the name of the producing
#' function for the object. Set automatically by using the function [set_RLum].
#'
#' @slot info
#' Object of class [list] for additional information on the object itself
#'
#' @slot .uid
#' Object of class [character] for a unique object identifier. This id is
#' usually calculated using the internal function `create_UID()` if the function [set_RLum]
#' is called.
#'
#' @slot .pid
#' Object of class [character] for a parent id. This allows nesting RLum-objects
#' at will. The parent id can be the uid of another object.
#'
#' @note `RLum` is a virtual class.
#'
#' @section Objects from the Class:
#' A virtual Class: No objects can be created from it.
#'
#' @section Class version: 0.4.0
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Data-class], [RLum.Data.Curve-class], [RLum.Data.Spectrum-class], [RLum.Data.Image-class],
#' [RLum.Analysis-class], [RLum.Results-class], [methods_RLum]
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
             info = "list",
             .uid = "character",
             .pid = "character"
             ),
           contains = "VIRTUAL",
           prototype = prototype(
             originator = NA_character_,
             info = list(),
             .uid = NA_character_,
             .pid = NA_character_
           )
         )


## replicate_RLum() ---------------------------------------------------------
#' @describeIn replicate_RLum
#' Replication method for [RLum-class] objects.
#'
#' @export
setMethod(
  "replicate_RLum",
  "RLum",
  definition = function(object, times = 1) {
    lapply(1:times, function(x) object)
  }
)
