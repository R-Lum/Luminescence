#' General set function for RLum S4 class objects
#'
#' Function calls object-specific set functions for RLum S4 class objects.
#'
#' The function provides a generalised access point for specific
#' \code{\linkS4class{RLum}} objects.\cr Depending on the given class, the
#' corresponding method to create an object from this class will be selected.
#' Allowed additional arguments can be found in the documentations of the
#' corresponding \code{\linkS4class{RLum}} class: \code{\linkS4class{RLum.Data.Curve}},
#' \code{\linkS4class{RLum.Data.Image}}, \code{\linkS4class{RLum.Data.Spectrum}},
#' \code{\linkS4class{RLum.Analysis}} and \code{\linkS4class{RLum.Results}}
#'
#' @param class \code{\linkS4class{RLum}} (\bold{required}): name of the S4 class to
#' create
#'
#' @param originator \code{\link{character}} (automatic): contains the name of the calling function
#' (the function that produces this object); can be set manually.
#'
#' @param .uid \code{\link{character}} (automatic): sets an unique ID for this object
#' using the internal C++ function \code{.create_UID}.
#'
#' @param .pid \code{\link{character}} (with default): option to provide a parent id for nesting
#' at will.
#'
#' @param \dots further arguments that one might want to pass to the specific
#' set method
#'
#' @return Returns an object of the specified class.
#'
#' @section Function version: 0.3.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#' (France)
#'
#' @seealso
#' \code{\linkS4class{RLum.Data.Curve}},
#' \code{\linkS4class{RLum.Data.Image}},
#' \code{\linkS4class{RLum.Data.Spectrum}},
#' \code{\linkS4class{RLum.Analysis}},
#' \code{\linkS4class{RLum.Results}}
#'
#' @keywords utilities
#'
#' @examples
#'
#' ##produce empty objects from each class
#' set_RLum(class = "RLum.Data.Curve")
#' set_RLum(class = "RLum.Data.Spectrum")
#' set_RLum(class = "RLum.Data.Spectrum")
#' set_RLum(class = "RLum.Analysis")
#' set_RLum(class = "RLum.Results")
#'
#' ##produce a curve object with arbitrary curve values
#' object <- set_RLum(
#' class = "RLum.Data.Curve",
#' curveType = "arbitrary",
#' recordType = "OSL",
#' data = matrix(c(1:100,exp(-c(1:100))),ncol = 2))
#'
#' ##plot this curve object
#' plot_RLum(object)
#'
#' @export
setGeneric("set_RLum", function (class, originator, .uid = .create_UID(), .pid = NA_character_, ... ) {
  class(class) <- as.character(class)

  if(missing(originator)) {
    if (is(sys.call(which = -1)[[1]], "name")) {
      originator <- as.character(sys.call(which = -1)[[1]])
    } else{
      originator <- NA_character_
    }
  }

  standardGeneric("set_RLum")
})

