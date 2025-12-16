#' Class `"RLum"`
#'
#' Abstract class for data in the package Luminescence
#' Subclasses are:
#'
#' **RLum-class**\cr
#' |\cr
#' |----[Luminescence::RLum.Data-class]\cr
#' |----|-- [Luminescence::RLum.Data.Curve-class]\cr
#' |----|-- [Luminescence::RLum.Data.Spectrum-class]\cr
#' |----|-- [Luminescence::RLum.Data.Image-class]\cr
#' |----[Luminescence::RLum.Analysis-class]\cr
#' |----[Luminescence::RLum.Results-class]
#'
#' @name RLum-class
#'
#' @docType class
#'
#' @slot originator
#' Object of class [character] containing the name of the producing
#' function for the object. Set automatically by using the function [Luminescence::set_RLum].
#'
#' @slot info
#' Object of class [list] for additional information on the object itself
#'
#' @slot .uid
#' Object of class [character] for a unique object identifier. This id is
#' usually calculated using the internal function `create_UID` if the function [Luminescence::set_RLum]
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
#' @seealso [Luminescence::RLum.Data-class], [Luminescence::RLum.Data.Curve-class],
#' [Luminescence::RLum.Data.Spectrum-class], [Luminescence::RLum.Data.Image-class],
#' [Luminescence::RLum.Analysis-class], [Luminescence::RLum.Results-class], [Luminescence::methods_RLum]
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
#' Replication method for [Luminescence::RLum-class] objects.
#'
#' @export
setMethod(
  "replicate_RLum",
  "RLum",
  definition = function(object, times = 1) {
    lapply(1:times, function(x) object)
  }
)
