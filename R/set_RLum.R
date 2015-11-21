#' General set function for RLum S4 class objects
#'
#' Function calls object-specific set functions for RLum S4 class objects.
#'
#' The function provides a generalised access point for specific
#' \code{\linkS4class{RLum}} objects.\cr Depending on the input object, the
#' corresponding set function will be selected.  Allowed arguments can be found
#' in the documentations of the corresponding \code{\linkS4class{RLum}} class.
#'
#' @param class \code{\linkS4class{RLum}} (\bold{required}): name of the S4 class to
#' create
#'
#' @param originator \code{\link{character}} (automatic): contains the name of the calling function
#' (the function that produces this object); can be set manually.
#'
#' @param \dots further arguments that one might want to pass to the specific
#' get function
#'
#' @return Return is the same as input objects as provided in the list.
#'
#' @section Function version: 0.2.0
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
#' @aliases set_RLum.Data.Curve set_RLum.Data.Image set_RLum.Data.Spectrum
#' set_RLum.Analysis set_RLum.Results
#'
#' @export
setGeneric("set_RLum", function (class, originator, ... ) {
  class(class) <- as.character(class)

  if(missing(originator)) {
    if (is(sys.call(which = -1)[[1]], "name")) {
      originator <- as.character(sys.call(which = -1)[[1]])
    } else{
      originator <- "NA"
    }
  }
  standardGeneric("set_RLum")
})


## ---- DEPRECATED GENERICS
# .Deprecated in package version 0.5.0
# .Defunct in 0.5.X
# Removed in 0.6.0

#' @noRd
#' @export
set_RLum.Analysis <- function(...) {
  .Deprecated("set_RLum")

  if(missing(originator)) {
    if (is(sys.call(which = -1)[[1]], "name")) {
      originator <- as.character(sys.call(which = -1)[[1]])
    } else{
      originator <- "NA"
    }
  }

  set_RLum("RLum.Analysis", ...)
}

#' @noRd
#' @export
set_RLum.Data.Curve <- function(...) {
  .Deprecated("set_RLum")

  if(missing(originator)) {
    if (is(sys.call(which = -1)[[1]], "name")) {
      originator <- as.character(sys.call(which = -1)[[1]])
    } else{
      originator <- "NA"
    }
  }

  set_RLum("RLum.Data.Curve", ...)
}

#' @noRd
#' @export
set_RLum.Data.Image <- function(...) {
  .Deprecated("set_RLum")

  if(missing(originator)) {
    if (is(sys.call(which = -1)[[1]], "name")) {
      originator <- as.character(sys.call(which = -1)[[1]])
    } else{
      originator <- "NA"
    }
  }

  set_RLum("RLum.Data.Image", ...)
}

#' @noRd
#' @export
set_RLum.Data.Spectrum <- function(...) {
  .Deprecated("set_RLum")

  if(missing(originator)) {
    if (is(sys.call(which = -1)[[1]], "name")) {
      originator <- as.character(sys.call(which = -1)[[1]])
    } else{
      originator <- "NA"
    }
  }

  set_RLum("RLum.Data.Spectrum", ...)
}

#' @noRd
#' @export
set_RLum.Results <- function(originator,...) {
  .Deprecated("set_RLum")

  if(missing(originator)) {
    if (is(sys.call(which = -1)[[1]], "name")) {
      originator <- as.character(sys.call(which = -1)[[1]])
    } else{
      originator <- "NA"
    }
  }
  set_RLum(class = "RLum.Results", originator = originator, ...)
}
