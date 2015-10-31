#' @include get_RLum.R set_RLum.R names_RLum.R length_RLum.R
NULL

#' Class \code{"RLum.Data.Curve"}
#'
#' Class for luminescence curve data.
#'
#' @name RLum.Data.Curve-class
#'
#' @docType class
#'
#' @slot recordType Object of class "character" containing the type of the curve (e.g. "TL" or "OSL")
#'
#' @slot curveType Object of class "character" containing curve type, allowed values are measured or predefined
#'
#' @slot data Object of class \code{\link{matrix}} containing curve x and y data.
#' 'data' can also be of type \code{RLum.Data.Curve} to change object values without deconstructing the object.
#' For example: #' \code{set_RLum(class = 'RLum.Data.Curve', data = Your.RLum.Data.Curve, recordType = 'never seen before')}
#' would just change the recordType. Missing arguements  the value is taken from the input object
#' in 'data' (which is already an RLum.Data.Curve object in this example)
#'
#'
#' @slot info Object of class "list" containing further meta information objects
#'
#' @note The class should only contain data for a single curve. For additional
#' elements the slot \code{info} can be used (e.g. providing additional heating
#' ramp curve).
#'
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RLum.Data.Curve", ...)}.
#'
#' @section Class version: 0.2.1
#'
#' @return Returns:\cr
#'
#' \bold{\code{set_RLum}}\cr
#'
#' Returns an \code{\linkS4class{RLum.Data.Curve}} object.
#'
#' \bold{\code{get_RLum}}\cr
#'
#' Returns:\cr
#' (1) A matrix with the curve values or \cr
#' (2) only the info object if \code{info.object} was set.\cr
#'
#' \bold{\code{merge_RLum}}\cr
#'
#' Returns a new \code{\linkS4class{RLum.Data.Curve}} object.
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
#' showClass("RLum.Data.Curve")
#'
#' @export
setClass("RLum.Data.Curve",
         slots = list(
           recordType = "character",
           curveType = "character",
           data = "matrix",
           info = "list"
           ),
         contains = "RLum.Data",
         prototype = list (
           recordType = character(),
           curveType = character(),
           data = matrix(0,0,2),
           info = list()
           )
         )


# setAs - coerce methods ------------------------------------------------------

##----------------------------------------------
##COERCE FROM AND TO data.frame

setAs("data.frame", "RLum.Data.Curve",
      function(from,to){

              new(to,
                  recordType = "unkown curve type",
                  curveType = "NA",
                  data = as.matrix(from),
                  info = list())
            })

setAs("RLum.Data.Curve", "data.frame",
      function(from){

        data.frame(x = from@data[,1],
                   y = from@data[,2])

      })


##----------------------------------------------
##COERCE FROM AND TO matrix

setAs("matrix", "RLum.Data.Curve",
      function(from,to){

        new(to,
            recordType = "unkown curve type",
            curveType = "NA",
            data = from,
            info = list())

      })

setAs("RLum.Data.Curve", "matrix",
      function(from){

        from@data

      })




# show method for object ------------------------------------------------------
#' @describeIn RLum.Data.Curve
#' Show structure of RLum and Risoe.BINfile class objects
#' @export
setMethod("show",
          signature(object = "RLum.Data.Curve"),
          function(object){


            ##print information

            cat("\n [RLum.Data.Curve]")
            cat("\n\t recordType:", object@recordType)
            cat("\n\t curveType:",  object@curveType)
            cat("\n\t measured values:", length(object@data[,1]))
            cat("\n\t .. range of x-values:", suppressWarnings(range(object@data[,1])))
            cat("\n\t .. range of y-values:",  suppressWarnings(range(object@data[,2])))
            cat("\n\t additional info elements:", length(object@info))
            #cat("\n\t\t >> names:", names(object@info))
          }
)




# constructor (set) method for object class -----------------------------------

#' @describeIn RLum.Data.Curve
#' Construction method for RLum.Data.Curve object. The slot info is optional
#' and predefined as empty list by default.
#'
#' @param class [\code{set_RLum}] \code{\link{character}} (\bold{required}): name of the \code{RLum} class to create
#' @param recordType [\code{set_RLum}] \code{\link{character}} (optional): record type (e.g., "OSL")
#' @param curveType [\code{set_RLum}] \code{\link{character}} (optional): curve type (e.g., "predefined" or "measured")
#' @param data [\code{set_RLum}] \code{\link{matrix}} (\bold{required}): raw curve data
#' @param info [\code{set_RLum}] \code{\link{list}} (optional): info elements
#'
#' @export
setMethod(
  "set_RLum",
  signature = signature("RLum.Data.Curve"),

  definition = function(class,
                        recordType = character(),
                        curveType = character(),
                        data = matrix(0,0,2),
                        info = list()) {
    ##with this RLum.Data.Curve objects can be provided to be reconstructed
    if (is(data, "RLum.Data.Curve")) {

      ##check for missing curveType
      if (missing(curveType)) {
        curveType <- data@curveType

      }

      ##check for missing recordType
      if(missing(recordType)){
        recordType <- data@recordType

      }

      ##check for missing data ... not possible as data is the object itself

      ##check for missing info
      if(missing(info)){
       info <- data@info

      }

      new(
        "RLum.Data.Curve",
        recordType = recordType,
        curveType = curveType,
        data = data@data,
        info = info
      )

    }else{
      ##check for missing curveType
      if (missing(curveType)) {
        curveType <- "NA"

      }

      ##check for missing recordType
      if (missing(recordType)) {
        recordType <- "NA"

      }


      new(
        "RLum.Data.Curve",
        recordType = recordType,
        curveType = curveType,
        data = data,
        info = info
      )

    }

  }
)

# constructor (get) method for object class -----------------------------------

#' @describeIn RLum.Data.Curve
#' Accessor method for RLum.Data.Curve object. The argument info.object is
#' optional to directly access the info elements. If no info element name is
#' provided, the raw curve data (matrix) will be returned.
#'
#' @param object [\code{get_RLum}] an object of class \code{\linkS4class{RLum.Data.Curve}} (\bold{required})
#' @param info.object [\code{get_RLum}] \code{\link{character}} (optional): name of the wanted info
#' element
#'
#' @export
setMethod("get_RLum",
          signature("RLum.Data.Curve"),
          definition = function(object, info.object) {

           ##Check if function is of type RLum.Data.Curve
           if(is(object, "RLum.Data.Curve") == FALSE){

              stop("[get_RLum] Function valid for 'RLum.Data.Curve' objects only!")

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


# names method for object class ------------------------------------------

#' @describeIn RLum.Data.Curve
#' Returns the length of the curve object, which is the maximum of the
#' value time/temperature of the curve (corresponding to the stimulation length)
#'
#' @export
setMethod("length_RLum",
          "RLum.Data.Curve",
          function(object){
            max(object@data[,1])

          })


# names method for object class ------------------------------------------

#' @describeIn RLum.Data.Curve
#' Returns the names info elements coming along with this curve object
#'
#' @export
setMethod("names_RLum",
          "RLum.Data.Curve",
          function(object){
            names(object@info)

          })
