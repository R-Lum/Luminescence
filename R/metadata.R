#' @title Safe replacement of object metadata
#'
#' @description
#' Generic function for replacement of object metadata.
#'
#' @param object (**required**) object to manipulate
#'
#' @param ... further arguments passed to the function
#'
#' @param value the value assigned
#'
#' @author
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#'
#' @keywords utilities
#'
#' @md
#' @export
setGeneric("replace_metadata<-",
           function (object, ..., value) standardGeneric("replace_metadata<-"))
