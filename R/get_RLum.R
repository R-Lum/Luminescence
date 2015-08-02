#' General accessor function for RLum S4 class objects
#'
#' Function calls object-specific get functions for RLum S4 class objects.
#'
#' The function provides a generalised access point for specific
#' \code{\linkS4class{RLum}} objects.\cr Depending on the input object, the
#' corresponding get function will be selected. Allowed arguments can be found
#' in the documentations of the corresponding \code{\linkS4class{RLum}} class.
#'
#' @param object \code{\linkS4class{RLum}} (\bold{required}): S4 object of
#' class \code{RLum}
#'
#' @param \dots further arguments that will be passed to the object specific methods. For
#' furter details on the supported arguments please see the class
#' documentation: \code{\linkS4class{RLum.Data.Curve}},
#' \code{\linkS4class{RLum.Data.Spectrum}}, \code{\linkS4class{RLum.Data.Image}},
#' \code{\linkS4class{RLum.Analysis}} and \code{\linkS4class{RLum.Results}}
#'
#' @return Return is the same as input objects as provided in the list.
#'
#' @section Function version: 0.1
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
#' @aliases get_RLum.Data.Curve get_RLum.Data.Image get_RLum.Data.Spectrum
#' get_RLum.Analysis get_RLum.Results
#'
#' @examples
#'
#'
#' ##Example based using data and from the calc_CentralDose() function
#'
#' ##load example data
#' data(ExampleData.DeValues, envir = environment())
#'
#' ##apply the central dose model 1st time
#' temp1 <- calc_CentralDose(ExampleData.DeValues$CA1)
#'
#' ##get results and store them in a new object
#' temp.get <- get_RLum(object = temp1)
#'
#'
#'
setGeneric("get_RLum", function (object, ...) { standardGeneric("get_RLum") })


## ---- DEPRECATED GENERICS
# .Deprecated in package version 0.5.0
# .Defunct in 0.5.X
# Removed in 0.6.0

#' @noRd
get_RLum.Analysis <- function(...) {
  .Deprecated("get_RLum")
  get_RLum(...)
}

#' @noRd
get_RLum.Data.Curve <- function(...) {
  .Deprecated("get_RLum")
  get_RLum(...)
}

#' @noRd
get_RLum.Data.Image <- function(...) {
  .Deprecated("get_RLum")
  get_RLum(...)
}

#' @noRd
get_RLum.Data.Spectrum <- function(...) {
  .Deprecated("get_RLum")
  get_RLum(...)
}

#' @noRd
get_RLum.Results <- function(...) {
  .Deprecated("get_RLum")
  get_RLum(...)
}
