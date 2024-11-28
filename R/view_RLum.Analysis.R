#' @title Data view function
#'
#' @description
#' Data view function that sits on [utils::View]
#'
#' @param object [RLum.Analysis-class] (**required**):
#'
#' @param ... further arguments
#'
#' @return NULL and a view window
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Analysis-class]
#'
#' @keywords utilities internal
#'
#'@md
#'@export
setGeneric(
  name = "view_RLum.Analysis",
  def = function(object, ...) {
    standardGeneric("view_RLum.Analysis")
  },
  package = "Luminescence"
)
