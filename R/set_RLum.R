#' General set function for RLum S4 class objects
#'
#' Function calls object-specific set functions for RLum S4 class objects.
#'
#' The function provides a generalised access point for specific
#' [RLum-class] objects.\cr 
#' Depending on the given class, the corresponding method to create an object 
#' from this class will be selected. Allowed additional arguments can be found 
#' in the documentations of the corresponding [RLum-class] class: 
#' - [RLum.Data.Curve-class],
#' - [RLum.Data.Image-class], 
#' - [RLum.Data.Spectrum-class],
#' - [RLum.Analysis-class], 
#' - [RLum.Results-class]
#'
#' @param class [RLum-class] (**required**): 
#' name of the S4 class to create
#'
#' @param originator [character] (*automatic*): 
#' contains the name of the calling function (the function that produces this object); 
#' can be set manually.
#'
#' @param .uid [character] (*automatic*): 
#' sets an unique ID for this object using the internal C++ function `create_UID`.
#'
#' @param .pid [character] (*with default*): 
#' option to provide a parent id for nesting at will.
#'
#' @param ... further arguments that one might want to pass to the specific set method
#'
#' @return 
#' Returns an object of the specified class.
#'
#' @section Function version: 0.3.0
#'
#' @author 
#' Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @seealso [RLum.Data.Curve-class], [RLum.Data.Image-class], 
#' [RLum.Data.Spectrum-class], [RLum.Analysis-class], [RLum.Results-class]
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
#' @md
#' @export
setGeneric("set_RLum", function (class, originator, .uid = create_UID(), .pid = NA_character_, ... ) {
  class(class) <- as.character(class)

  if(missing(originator)) {
    if (is(sys.call(which = -1)[[1]], "language")) {
      originator <- as.character(sys.call(which = -1)[[1]])

      ##account for calls using the double colons, in this case the vector is
      ##of length 3, not only 1
      if(length(originator) == 3){
        originator <- originator[3]
      }

    } else{
      originator <- NA_character_
    }
  }

  standardGeneric("set_RLum")
})

