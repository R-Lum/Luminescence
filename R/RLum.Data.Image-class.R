#' @include get_RLum.R set_RLum.R names_RLum.R
NULL

#' Class \code{"RLum.Data.Image"}
#'
#' Class for luminescence image data (TL/OSL/RF).
#'
#' @name RLum.Data.Image-class
#'
#' @docType class
#'
#' @slot recordType Object of class "character" containing the type of the curve (e.g. "OSL image", "TL image")
#'
#' @slot curveType Object of class "character" containing curve type, allowed values are measured or predefined
#'
#' @slot data Object of class "RasterBrick" containing images (raster data).
#'
#' @slot info Object of class "list" containing further meta information objects
#'
#' @note The class should only contain data for a set of images. For additional
#' elements the slot \code{info} can be used.
#'
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{set_RLum("RLum.Data.Image", ...)}.
#'
#' @section Class version: 0.2.1
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @seealso \code{\linkS4class{RLum}}, \code{\linkS4class{RLum.Data}},
#' \code{\link{plot_RLum}}
#'
#' @keywords classes
#'
#' @examples
#'
#' showClass("RLum.Data.Image")
#'
#' ##so far no further example available
#'
#' @importClassesFrom raster RasterBrick
#' @export
setClass(
  "RLum.Data.Image",
  slots = list(
    recordType = "character",
    curveType = "character",
    data = "RasterBrick",
    info = "list"
  ),
  contains = "RLum.Data",
  prototype = list (
    recordType = character(),
    curveType = character(),
    data = raster::brick(raster::raster(matrix())),
    info = list()
  )
)


####################################################################################################
###as()
####################################################################################################

##DATA.FRAME
##COERCE RLum.Data.Image >> data.frame AND data.frame >> RLum.Data.Image
#' as()
#'
#' for \code{[RLum.Data.Image]}
#'
#' \bold{[RLum.Data.Image]}\cr
#'
#' \tabular{ll}{
#'  \bold{from} \tab \bold{to}\cr
#'   \code{data.frame} \tab \code{data.frame}\cr
#'   \code{matrix} \tab \code{matrix}
#'
#' }
#'
#' @name as
#'
#'
setAs("data.frame", "RLum.Data.Image",
      function(from,to){

        new(to,
            recordType = "unkown curve type",
            curveType = "NA",
            data = as.matrix(from),
            info = list())
      })

setAs("RLum.Data.Image", "data.frame",
      function(from){

        data.frame(x = from@data@values[seq(1,length(from@data@values), by = 2)],
                   y = from@data@values[seq(2,length(from@data@values), by = 2)])

      })


##MATRIX
##COERCE RLum.Data.Image >> matrix AND matrix >> RLum.Data.Image
setAs("matrix", "RLum.Data.Image",
      function(from,to){

        new(to,
            recordType = "unkown curve type",
            curveType = "NA",
            data = raster::brick(raster::raster(as.matrix(from))),
            info = list())
      })

setAs("RLum.Data.Image", "matrix",
      function(from){

        ##only the first object is convertec
        as.matrix(from[[1]])

      })


####################################################################################################
###show()
####################################################################################################
#' @describeIn RLum.Data.Image
#' Show structure of RLum and Risoe.BINfile class objects
#' @export
setMethod("show",
          signature(object = "RLum.Data.Image"),
          function(object){

            x.rows <- object@data@ncols
            y.cols <- object@data@nrows
            z.range <- paste(min(object@data@data@min),":",max(object@data@data@max))

            ##print information

            cat("\n [RLum.Data.Image]")
            cat("\n\t recordType:", object@recordType)
            cat("\n\t curveType:",  object@curveType)
            cat("\n\t .. recorded frames:", length(object@data@data@names))
            cat("\n\t .. .. pixel per frame:", x.rows*y.cols)
            cat("\n\t .. .. x dimension [px]:", x.rows)
            cat("\n\t .. .. y dimension [px]:", y.cols)
            cat("\n\t .. .. full pixel value range:", z.range)
            cat("\n\t additional info elements:", length(object@info))
            #cat("\n\t\t >> names:", names(object@info))
          }
)


####################################################################################################
###set_RLum()
####################################################################################################
#' @describeIn RLum.Data.Image
#' Construction method for RLum.Data.Image object. The slot info is optional
#' and predefined as empty list by default..
#'
#' @param class \code{\link{character}}: name of the \code{RLum} class to create
#' @param originator \code{\link{character}} (automatic): contains the name of the calling function
#' (the function that produces this object); can be set manually.
#' @param recordType \code{\link{character}}: record type (e.g. "OSL")
#' @param curveType \code{\link{character}}: curve type (e.g. "predefined" or "measured")
#' @param data \code{\link{matrix}}: raw curve data
#' @param info \code{\link{list}}: info elements
#'
#' @export
setMethod("set_RLum",
          signature = signature("RLum.Data.Image"),

          definition = function(class, originator, recordType, curveType, data, info){

            ##check for missing curveType
            if(missing(curveType)==TRUE){

              curveType <- "NA"

            }else if (is(curveType, "character") == FALSE){

              stop("[set_RLum()] Error: 'curveType' has to be of type 'character'!")

            }

            ##check for missing arguments
            if(missing(recordType) | missing(data)){

              temp.error.missing <- paste(c(

                if(missing(recordType)){"'recordType'"}else{},
                if(missing(data)){"'data'"}else{}),
                collapse=", ")

              ##set error message
              temp.error.message <- paste("[set_RLum()] Missing required arguments " ,
                                          temp.error.missing,"!", sep="")
              stop(temp.error.message)
            }

            ##handle missing info argument
            if(missing(info)){

              info <- list()

            }else if (is(info, "list") == FALSE){

              stop("[set_RLum()] 'info' has to be of type 'list'!")

            }

            new("RLum.Data.Image",
                originator = originator,
                recordType = recordType,
                curveType = curveType,
                data = data,
                info = info)

          })

####################################################################################################
###get_RLum()
####################################################################################################
#' @describeIn RLum.Data.Image
#' Accessor method for RLum.Data.Image object. The argument info.object is
#'  optional to directly access the info elements. If no info element name is
#'  provided, the raw image data (RasterBrick) will be returned.
#'
#' @param object an object of class \code{\linkS4class{RLum.Data.Image}}
#' @param info.object object of class "list" containing further meta information objects
#'
#' @export
setMethod("get_RLum",
          signature("RLum.Data.Image"),
          definition = function(object, info.object) {

            ##Check if function is of type RLum.Data.Image
            if(is(object, "RLum.Data.Image") == FALSE){

              stop("[get_RLum] Function valid for 'RLum.Data.Image' objects only!")

            }

            ##if missing info.object just show the curve values

            if(missing(info.object) == FALSE){

              if(is(info.object, "character") == FALSE){
                stop("[get_RLum] Error: 'info.object' has to be a character!")
              }

              if(info.object %in% names(object@info) == TRUE){

                unlist(object@info[info.object])

              }else{

                ##grep names
                temp.element.names <- paste(names(object@info), collapse = ", ")

                stop.text <- paste("[get_RLum] Error: Invalid element name. Valid names are:", temp.element.names)

                stop(stop.text)

              }


            }else{

              object@data

            }
          })

####################################################################################################
###names_RLum()
####################################################################################################
#' @describeIn RLum.Data.Image
#' Returns the names info elements coming along with this curve object
#'
#' @export
setMethod("names_RLum",
          "RLum.Data.Image",
          function(object) {
            names(object@info)

          })
