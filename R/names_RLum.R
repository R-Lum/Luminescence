#' S4-names function for RLum S4 class objects
#'
#' Function calls object-specific names functions for RLum S4 class objects.
#'
#' The function provides a generalised access point for specific
#' [RLum-class] objects.\cr
#' Depending on the input object, the corresponding 'names' function will be
#' selected. Allowed arguments can be found in the documentations of the
#' corresponding [RLum-class] class.
#'
#' @param object [RLum-class] (**required**):
#' S4 object of class `RLum`
#'
#' @return Returns a [character]
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#' @seealso [RLum.Data.Curve-class], [RLum.Data.Image-class],
#' [RLum.Data.Spectrum-class], [RLum.Analysis-class], [RLum.Results-class]
#'
#' @keywords utilities
#'
#' @aliases names_RLum
#'
#' @md
#' @export
setGeneric("names_RLum", function(object) {
  standardGeneric("names_RLum")
})

# Method for names_RLum method for RLum objects in a list for a list of objects  -------------
#' @describeIn names_RLum
#' Returns a list of [RLum-class] objects that had been passed to [names_RLum]
#'
#'
#' @md
#' @export
setMethod("names_RLum",
          signature = "list",
          function(object) {
            ##apply method in the objects and return the same
            lapply(object, function(x) {
              if (inherits(x, "RLum")) {
                return(names_RLum(x))
              } else{
                return(x)
              }

            })

          })
