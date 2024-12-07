#' @title Safe replacement of object metadata
#'
#' @description
#' Generic function for replacement of object metadata in [Risoe.BINfileData-class]
#' and [RLum-class] objects
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
#' ## (1) Replace for Risoe.BINfileData
#' ## load example data
#' data(ExampleData.BINfileData, envir = environment())
#'
#' ## show data
#' CWOSL.SAR.Data
#'
#' ## replace all LTYPE to RSL
#' ## but only for the first position
#' replace_metadata(
#'  object = CWOSL.SAR.Data,
#'  info_element = "LTYPE",
#'  subset = (POSITION == 1)) <- "RSL"
#' CWOSL.SAR.Data
#'
#' @md
#' @export
setGeneric("replace_metadata<-",
           function (object, ..., value) standardGeneric("replace_metadata<-"))
