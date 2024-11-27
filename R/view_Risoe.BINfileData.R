#' @title Data view function
#'
#' @description
#' Data view function that sits on [utils::View]
#'
#' @param object [Risoe.BINfileData-class] (**required**):
#'
#' @param ... further arguments
#'
#' @return NULL and a view window
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [Risoe.BINfileData-class]
#'
#' @keywords utilities internal
#'
#'@md
#'@export
setGeneric(
  name = "view_Risoe.BINfileData",
  def = function(object, ...) {
    standardGeneric("view_Risoe.BINfileData")
  },
  package = "Luminescence"
)
