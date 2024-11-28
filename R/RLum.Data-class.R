#' @include view.R
NULL

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

## view() -------------------------------------------------------------------
#' @describeIn RLum.Data
#'
#' View method for [RLum.Data-class] objects
#'
#' @param object an object of class [RLum.Data-class]
#'
#' @param ... other arguments that might be passed
#'
#' @keywords internal
#'
#' @md
#' @export
setMethod("view",
          signature = "RLum.Data",
          definition = function(object, ...) {
  .set_function_name("view")
  on.exit(.unset_function_name(), add = TRUE)

  .validate_not_empty(object@info, name = "'info' slot")

    ## set title
    name <- list(...)$title
    if (is.null(name))
      name <- deparse(substitute(object))

    ## run view
    .view(x = object@info, title = name)
})
