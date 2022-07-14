#' @include get_RLum.R set_RLum.R names_RLum.R
NULL

#' Class `"RLum.Data.Image"`
#'
#' Class for representing luminescence image data (TL/OSL/RF). Such data are for example produced
#' by the function [read_SPE2R]
#'
#' @name RLum.Data.Image-class
#'
#' @docType class
#'
#' @slot recordType
#' Object of class [character] containing the type of the curve (e.g. "OSL image", "TL image")
#'
#' @slot curveType
#' Object of class [character] containing curve type, allowed values
#' are measured or predefined
#'
#' @slot data
#' Object of class [array] containing image data.
#'
#' @slot info
#' Object of class [list] containing further meta information objects
#'
#' @note
#' The class should only contain data for a set of images. For additional
#' elements the slot `info` can be used.
#'
#' @section Objects from the class:
#' Objects can be created by calls of the form `set_RLum("RLum.Data.Image", ...)`.
#'
#' @section Class version: 0.5.1
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum-class], [RLum.Data-class], [plot_RLum], [read_SPE2R], [read_TIFF2R]
#'
#' @keywords classes
#'
#' @examples
#'
#' showClass("RLum.Data.Image")
#'
#' ##create empty RLum.Data.Image object
#' set_RLum(class = "RLum.Data.Image")
#'
#' @md
#' @export
setClass(
  "RLum.Data.Image",
  slots = list(
    recordType = "character",
    curveType = "character",
    data = "array",
    info = "list"
  ),
  contains = "RLum.Data",
  prototype = list (
    recordType = character(),
    curveType = character(),
    data = array(),
    info = list()
  )
)

# as() ----------------------------------------------------------------------------------------
##DATA.FRAME
##COERCE RLum.Data.Image >> data.frame AND data.frame >> RLum.Data.Image
#' as()
#'
#' for `[RLum.Data.Image-class]`
#'
#' **[RLum.Data.Image-class]**
#'
#' \tabular{ll}{
#'  **from** \tab **to**\cr
#'   `data.frame` \tab `data.frame`\cr
#'   `matrix` \tab `matrix`
#' }
#'
#' @md
#' @name as
## from data.frame ----
setAs("data.frame", "RLum.Data.Image",
      function(from,to){
        new(to,
            recordType = "unkown curve type",
            curveType = "NA",
            data = array(unlist(from), dim = c(nrow(from),ncol(from),1)),
            info = list())
      })

## to data.frame ----
setAs("RLum.Data.Image", "data.frame",
        function(from){
          if(dim(from@data)[3] == 1) {
             as.data.frame(from@data[,,1])

          } else {
            stop("No viable coercion to data.frame, object contains multiple frames.",
                 call. = FALSE)

          }
        })


## from matrix   ----
setAs("matrix", "RLum.Data.Image",
      function(from,to){
        new(to,
            recordType = "unkown curve type",
            curveType = "NA",
            data = array(from, c(nrow(from), ncol(from), 1)),
            info = list())
      })

## to matrix ----
setAs("RLum.Data.Image", "matrix",
      function(from){
        if(dim(from@data)[3] == 1) {
          from@data[,,1, drop = TRUE]
        } else {
         stop("No viable coercion to matrix, object contains multiple frames. Please convert to array instead.", call. = FALSE)

        }
      })

## from array ----
setAs("array", "RLum.Data.Image",
      function(from, to){
        new(to,
            recordType = "unkown curve type",
            curveType = "NA",
            data = from,
            info = list())

      })

## to array ----
setAs("RLum.Data.Image", "array",
      function(from) from@data)


## from list ----
setAs("list", "RLum.Data.Image",
      function(from, to){
        array_list <- lapply(from, function(x) array(unlist(as.vector(x)), c(nrow(x), ncol(x), 1)))

        new(to,
            recordType = "unkown curve type",
            curveType = "NA",
            data = array(unlist(array_list),
                         c(nrow(array_list[[1]]), ncol(array_list[[1]]), length(array_list))),
            info = list())

      })

## to list ----
setAs("RLum.Data.Image", "list",
      function(from){
       lapply(1:dim(from@data)[3], function(x) from@data[,,x])

    })

# show() --------------------------------------------------------------------------------------
#' @describeIn RLum.Data.Image
#' Show structure of `RLum.Data.Image` object
#'
#' @keywords internal
#'
#' @md
#' @export
setMethod("show",
          signature(object = "RLum.Data.Image"),
          function(object){

            ## get dimension
            dim <- dim(object@data)

            ##print information
            cat("\n [RLum.Data.Image-class]")
            cat("\n\t recordType:", object@recordType)
            cat("\n\t curveType:",  object@curveType)
            cat("\n\t .. recorded frames:", max(1,dim[3], na.rm = TRUE))
            cat("\n\t .. .. pixel per frame:", dim[1]*dim[2])
            cat("\n\t .. .. x dimension [px]:", dim[1])
            cat("\n\t .. .. y dimension [px]:", dim[2])
            cat("\n\t .. .. full pixel value range:", paste(format(range(object@data), scientific = TRUE, digits = 2), collapse=" : "))
            cat("\n\t additional info elements:", length(object@info))
            #cat("\n\t\t >> names:", names(object@info))
          }
)



# set_RLum() ----------------------------------------------------------------------------------
#' @describeIn RLum.Data.Image
#' Construction method for RLum.Data.Image object. The slot info is optional
#' and predefined as empty list by default.
#'
#' @param class [`set_RLum`]; [character]: name of the `RLum` class to create
#'
#' @param originator [`set_RLum`]; [character] (*automatic*):
#' contains the name of the calling function (the function that produces this object);
#' can be set manually.
#'
#' @param .uid [`set_RLum`]; [character] (*automatic*):
#' sets an unique ID for this object using the internal C++ function `create_UID`.
#'
#' @param .pid [`set_RLum`]; [character] (*with default*):
#' option to provide a parent id for nesting at will.
#'
#' @param recordType [`set_RLum`]; [character]:
#' record type (e.g. "OSL")
#'
#' @param curveType [`set_RLum`]; [character]:
#' curve type (e.g. "predefined" or "measured")
#'
#' @param data [`set_RLum`]; [matrix]:
#' raw curve data. If data is of type `RLum.Data.Image` this can be used to
#' re-construct the object, i.e. modified parameters except `.uid` and `.pid`. The rest
#' will be subject to copy and paste unless provided.
#'
#' @param info [`set_RLum`]; [list]:
#' info elements
#'
#' @return
#'
#' **`set_RLum`**
#'
#' Returns an object from class `RLum.Data.Image`
#'
#' @md
#' @export
setMethod(
  "set_RLum",
  signature = signature("RLum.Data.Image"),
  definition = function(
    class,
    originator,
    .uid,
    .pid,
    recordType = "Image",
    curveType = NA_character_,
    data = array(),
    info = list()) {

    ##The case where an RLum.Data.Image object can be provided
    ##with this RLum.Data.Image objects can be provided to be reconstructed
    if (is(data, "RLum.Data.Image")) {
      ##check for missing curveType
      if (missing(curveType))
        curveType <- data@curveType

      ##check for missing recordType
      if (missing(recordType))
        recordType <- data@recordType

      ##check for missing data ... not possible as data is the object itself

      ##check for missing info
      if (missing(info))
        info <- data@info

      ##check for modified .uid & .pid
      ## >> this cannot be changed here, since both would be reset, by
      ## the arguments passed down from set_RLum() ... the generic function

      ##set empty class form object
      newRLumDataImage <- new("RLum.Data.Image")

      ##fill - this is the faster way, filling in new() costs ...
      newRLumDataImage@originator <- data@originator
      newRLumDataImage@recordType <- recordType
      newRLumDataImage@curveType <- curveType
      newRLumDataImage@data <- data@data
      newRLumDataImage@info <- info
      newRLumDataImage@.uid <- data@.uid
      newRLumDataImage@.pid <- data@.pid

    } else {
      ##set empty class from object
      newRLumDataImage <- new("RLum.Data.Image")

      ##fill - this is the faster way, filling in new() costs ...
      newRLumDataImage@originator <- originator
      newRLumDataImage@recordType <- recordType
      newRLumDataImage@curveType <- curveType
      newRLumDataImage@data <- data
      newRLumDataImage@info <- info
      newRLumDataImage@.uid <- .uid
      newRLumDataImage@.pid <- .pid
    }
    return(newRLumDataImage)
  }
)

# get_RLum() ----------------------------------------------------------------------------------
#' @describeIn RLum.Data.Image
#' Accessor method for `RLum.Data.Image` object. The argument `info.object` is
#' optional to directly access the info elements. If no info element name is
#' provided, the raw image data (`array`) will be returned.
#'
#' @param object [`get_RLum`], [`names_RLum`] (**required**):
#' an object of class [RLum.Data.Image-class]
#'
#' @param info.object [`get_RLum`]; [character]:
#' name of the info object to returned
#'
#' @return
#'
#' **`get_RLum`**
#'
#' 1. Returns the data object ([array])
#' 2. only the info object if `info.object` was set.
#'
#' @md
#' @export
setMethod("get_RLum",
          signature("RLum.Data.Image"),
          definition = function(object, info.object) {

            ##if missing info.object just show the curve values
            if(!missing(info.object)){
              if(!inherits(info.object, "character"))
                stop("[get_RLum] 'info.object' has to be a character!", call. = FALSE)

              if(info.object %in% names(object@info)){
                unlist(object@info[info.object])

              } else {
                stop(paste0(
                  "[get_RLum] Invalid element name. Valid names are: ",
                  paste(names(object@info), collapse = ", ")
                ),
                call. = FALSE)
             }
            } else {
              object@data
            }
          })



# names_RLum() --------------------------------------------------------------------------------
#' @describeIn RLum.Data.Image
#' Returns the names info elements coming along with this curve object
#'
#' @return
#'
#' **`names_RLum`**
#'
#' Returns the names of the info elements
#'
#' @md
#' @export
setMethod(
  "names_RLum",
  "RLum.Data.Image",
  function(object) names(object@info))

