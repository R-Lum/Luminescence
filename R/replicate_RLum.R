#' General replication function for RLum S4 class objects
#'
#' Function replicates RLum S4 class objects and returns a list for this objects
#'
#' @param object [RLum-class] (**required**):
#' an [RLum-class] object
#'
#' @param times [integer] (*optional*): 
#' number for times each element is repeated element
#'
#' @return Returns a [list] of the object to be repeated
#'
#' @section Function version: 0.1.0
#'
#' @author 
#' Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @seealso [RLum-class]
#'
#' @keywords utilities
#'
#' @md
#' @export
setGeneric("replicate_RLum", function (object, times = NULL) {
   standardGeneric("replicate_RLum")
})

