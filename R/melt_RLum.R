#' @title Melt RLum-class objects into a flat data.frame
#'
#' @param object [RLum-class] (**required**):
#' S4 object of class `RLum`
#'
#' @param ... further arguments passed to the specific class method
#'
#' @return
#' Returns a flat [data.frame]
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Data.Curve-class], [RLum.Analysis-class]
#'
#' @examples
#' data(ExampleData.XSYG, envir = environment())
#' melt_RLum(OSL.SARMeasurement[[2]][[1]])
#'
#' @keywords utilities
#'
#' @md
#' @export
setGeneric("melt_RLum", function(object, ...) {
  standardGeneric("melt_RLum")
  
})

# Method for melt_RLum method for RLum objects in a list for a list of objects  -------------------
#' @describeIn melt_RLum
#' Returns a list a single [data.frame]
#'
#' @md
#' @export
setMethod("melt_RLum",
          signature = "list",
          function(object, ...){
  ## silently remove non-RLum objects
  l <- .rm_nonRLum(object)
  
  ## just return NULL
  if(length(l) == 0)
    return(NULL)
              
  ## apply method in the objects and return the same
  l <- lapply(object, function(x) {
    t <- try(melt_RLum(x), silent = TRUE)
    
     if(inherits(t, "try-error"))
       return(NULL)
     else
       t
     })

    ## remove NULL
    l <- l[!vapply(l, is.null, logical(1))]
          
    ## now bind the data.frame
    as.data.frame(data.table::rbindlist(l))
            
})
