#' @include replicate_RLum.R RcppExports.R
NULL

#' Class \code{"RLum"}
#'
#' Abstract class for data in the package Luminescence
#'
#'
#' @name RLum-class
#'
#' @docType class
#'
#' @slot originator Object of class \code{\link{character}} containing the name of the producing
#' function for the object. Set automatically by using the function \code{\link{set_RLum}}.
#'
#' @slot info Object of class \code{\link{list}} for additional information on the object itself
#'
#' @slot .uid Object of class \code{\link{character}} for a unique object identifier. This id is
#' usually calculated using the internal function \code{.create_UID()} if the funtion \code{\link{set_RLum}}
#' is called.
#'
#' @slot .pid Object of class \code{\link{character}} for a parent id. This allows nesting RLum-objects
#' at will. The parent id can be the uid of another object.
#'
#' @note \code{RLum} is a virtual class.
#'
#' @section Objects from the Class: A virtual Class: No objects can be created
#' from it.
#'
#' @section Class version: 0.4.0
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


# replication method for object class ------------------------------------------

#' @describeIn RLum
#' Replication method RLum-objects
#'
#' @param object an object of class \code{\linkS4class{RLum}} (\bold{required})
#'
#' @param times \code{\link{integer}} (optional): number for times each element is repeated
#' element
#'
#' @export
setMethod(
  "replicate_RLum",
  "RLum",
  definition = function(object, times = NULL) {

    ##The case this is NULL
    if (is.null(times)) {
      times <- 1
    }

    lapply(1:times, function(x) {
      object

    })

  }
)
