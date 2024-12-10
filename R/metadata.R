#' @title Safe manipulation of object metadata
#'
#' @description
#' Generic functions for manipulation of metadata in [Risoe.BINfileData-class],
#' [RLum.Analysis-class] and [RLum.Data-class] objects.
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
#' @seealso [RLum.Data-class], [RLum.Analysis-class], [Risoe.BINfileData-class]
#'
#' @keywords utilities
#'
#' @examples
#'
#' ## load example data
#' data(ExampleData.BINfileData, envir = environment())
#'
#' ## show data
#' CWOSL.SAR.Data
#'
#' ## add a new field
#' add_metadata(CWOSL.SAR.Data,
#'              info_element = "INSTITUTE") <- "Heidelberg University"
#'
#' ## replace all LTYPE to RSL
#' ## but only for the first position
#' replace_metadata(
#'  object = CWOSL.SAR.Data,
#'  info_element = "LTYPE",
#'  subset = (POSITION == 1)) <- "RSL"
#'
#' ## show the modified data
#' CWOSL.SAR.Data
#'
#' @rdname metadata
#' @md
#' @export
setGeneric("add_metadata<-",
           function (object, ..., value) standardGeneric("add_metadata<-"))

#' @rdname metadata
#' @export
setGeneric("replace_metadata<-",
           function (object, ..., value) standardGeneric("replace_metadata<-"))
