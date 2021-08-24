#' Smoothing of data
#'
#' Function calls the object-specific smooth functions for provided RLum S4-class objects.
#'
#' The function provides a generalised access point for specific
#' [RLum-class] objects.\cr
#' Depending on the input object, the corresponding function will be selected.
#' Allowed arguments can be found in the documentations of the corresponding
#' [RLum-class] class. The smoothing is based on an internal function
#' called `.smoothing`.
#'
#' @param object [RLum-class] (**required**):
#' S4 object of class `RLum`
#'
#' @param ... further arguments passed to the specific class method
#'
#' @return
#' An object of the same type as the input object is provided
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#' @note
#' Currently only `RLum` objects of class `RLum.Data.Curve` and `RLum.Analysis`
#' (with curve data) are supported!
#'
#' @seealso [RLum.Data.Curve-class], [RLum.Analysis-class]
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
#' @md
#' @export
setGeneric("smooth_RLum", function(object, ...) {
  standardGeneric("smooth_RLum")

})

# Method for smooth_RLum method for RLum objects in a list for a list of objects  -------------------
#' @describeIn smooth_RLum
#' Returns a list of [RLum-class] objects that had been passed to [smooth_RLum]
#'
#'
#' @md
#' @export
setMethod("smooth_RLum",
signature = "list",
function(object, ...){

  ##apply method in the objects and return the same
  lapply(object, function(x){
    if(inherits(x, "RLum")){
      return(smooth_RLum(x,...))
    }else{
      return(x)
    }

  })

})
