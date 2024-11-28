#' @title Data view function
#'
#' @description
#' Data view function that sits on [utils::View]
#'
#' @param object [RLum.Results-class] (**required**):
#'
#' @param element [integer] (*with default*): index of the element to display
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
#' @seealso [RLum.Results-class]
#'
#' @keywords utilities internal
#'
#'@md
#'@export
setGeneric(
  name = "view_RLum.Results",
  def = function(object, element = 1, ...) {
    standardGeneric("view_RLum.Results")
  },
  package = "Luminescence"
)
