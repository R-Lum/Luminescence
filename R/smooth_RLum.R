#' Smoothing of data
#'
#' Function calls the object-specific smooth functions for provided RLum S4-class objects.
#'
#' The function provides a generalised access point for specific
#' \code{\linkS4class{RLum}} objects.\cr Depending on the input object, the
#' corresponding function will be selected. Allowed arguments can be found
#' in the documentations of the corresponding \code{\linkS4class{RLum}} class. The smoothing
#' is based on an internal function called \code{.smoothing}.
#'
#' @param object \code{\linkS4class{RLum}} (\bold{required}): S4 object of
#' class \code{RLum}
#'
#' @param ... further arguments passed to the specifc class method
#'
#' @return An object of the same type as the input object is provided
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#' (France)
#'
#' @note Currenlty only \code{RLum} objects of class \code{RLum.Data.Curve} and \code{RLum.Analysis} (with curve data) are supported!
#'
#' @seealso
#' \code{\linkS4class{RLum.Data.Curve}}, \code{\linkS4class{RLum.Analysis}}
#'
#' @examples
#'
#' ##load example data
#' data(ExampleData.CW_OSL_Curve, envir = environment())
#'
#' ##create RLum.Data.Curve object from this example
#' curve <-
#'   set_RLum(
#'       class = "RLum.Data.Curve",
#'       recordType = "OSL",
#'       data = as.matrix(ExampleData.CW_OSL_Curve)
#'   )
#'
#' ##plot data without and with smoothing
#' plot_RLum(curve)
#' plot_RLum(smooth_RLum(curve))
#'
#' @keywords utilities
#'
#' @export
setGeneric("smooth_RLum", function(object, ...) {
  standardGeneric("smooth_RLum")

})

# Method for smooth_RLum method for RLum objects in a list for a list of objects  -------------------
#' @describeIn smooth_RLum
#' Returns a list of \code{\linkS4class{RLum}} objects that had been passed to \code{\link{smooth_RLum}}
#'
#'
#' @export
setMethod("smooth_RLum",
signature = "list",
function(object, ...){

  ##apply method in the objects and return the sampe
  lapply(object, function(x){
    if(inherits(x, "RLum")){
      return(smooth_RLum(x,...))
    }else{
      return(x)
    }

  })

})
