#' @title Merge function for RLum.Analysis S4 class objects
#'
#' @description
#' This function simply allows to merge [RLum.Analysis-class]
#' objects. Moreover, other [RLum.Data-class] objects can be added
#' to an existing [RLum.Analysis-class] object. Supported objects
#' to be added are: [RLum.Data.Curve-class],
#' [RLum.Data.Spectrum-class] and
#' [RLum.Data.Image-class].
#'
#' @param objects [list] of [RLum.Analysis-class] (**required**):
#' list of S4 objects of class `RLum.Analysis`. Furthermore other objects of
#' class [RLum-class] can be added, see details.
#'
#' @return
#' Returns an [RLum.Analysis-class] object ordered according to the order
#' provided with the input list.
#'
#' @note
#' The information for the slot 'protocol' is taken from the first
#' [RLum.Analysis-class] object in the input list. Therefore at
#' least one object of type [RLum.Analysis-class] has to be provided.
#'
#' @section Function version: 0.2.1
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [merge_RLum], [RLum.Analysis-class], [RLum.Data.Curve-class],
#' [RLum.Data.Spectrum-class], [RLum.Data.Image-class], [RLum-class]
#'
#' @keywords utilities internal
#'
#' @examples
#'
#' ##merge different RLum objects from the example data
#' data(ExampleData.RLum.Analysis, envir = environment())
#' data(ExampleData.BINfileData, envir = environment())
#'
#' object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)
#' curve <- get_RLum(object)[[2]]
#'
#' temp.merged <- merge_RLum.Analysis(list(curve, IRSAR.RF.Data, IRSAR.RF.Data))
#'
#' @export
merge_RLum.Analysis<- function(
  objects
) {
  .set_function_name("merge_RLum.Analysis")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------
  .validate_class(objects, "list")
  .validate_not_empty(objects)

  ##check if object is of class RLum
  temp.class.test <- sapply(objects, function(x) {
    .validate_class(x, c("RLum.Analysis", "RLum.Data"),
                    name = "All elements of 'object'")
    class(x)[1]
  })

  ##check if at least one object of RLum.Analysis is provided
  if(!"RLum.Analysis"%in%temp.class.test){
    .throw_error("At least one input object in the list ",
                 "has to be of class 'RLum.Analysis'")
  }


  ## Merge objects ----------------------------------------------------------

  ##(0) get recent environment to later set variable temp.meta.data.first
  temp.environment  <- environment()
  temp.meta.data.first <- NULL

  ##(1) collect all elements in a list
  temp.element.list <- unlist(lapply(objects, function(x) {
    if (inherits(x, "RLum.Data"))
      return(x)

    ## x is an RLum.Analysis object
    ## extract meta data from the first RLum.Analysis object
    if (is.null(temp.meta.data.first)) {
      assign("temp.meta.data.first", x@protocol, envir = temp.environment)
    }

    get_RLum(x)
  }))

  ## return new RLum.Analysis object
  set_RLum(
    class = "RLum.Analysis",
    originator = "merge_RLum.Analysis",
    records = temp.element.list,
    protocol = temp.meta.data.first,
    info = unlist(lapply(objects, function(x) {
      x@info
    }), recursive = FALSE),
    .pid = unlist(lapply(objects, function(x) {
      x@.uid
    }))
  )
}
