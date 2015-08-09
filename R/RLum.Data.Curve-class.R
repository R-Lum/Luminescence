#' @include get_RLum.R set_RLum.R
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
#' @slot data Object of class "matrix" containing curve x and y data
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
#' @section Class version: 0.2.0
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

setMethod("show",
          signature(object = "RLum.Data.Curve"),
          function(object){


            ##print information

            cat("\n [RLum.Data.Curve]")
            cat("\n\t recordType:", object@recordType)
            cat("\n\t curveType:",  object@curveType)
            cat("\n\t measured values:", length(object@data[,1]))
            cat("\n\t .. range of x-values:", range(object@data[,1]))
            cat("\n\t .. range of y-values:", range(object@data[,2]))
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
#' @param recordType [\code{set_RLum}] \code{\link{character}} (optional): record type (e.g. "OSL")
#' @param curveType [\code{set_RLum}] \code{\link{character}} (optional): curve type (e.g. "predefined" or "measured")
#' @param data [\code{set_RLum}] \code{\link{matrix}} (\bold{required}): raw curve data
#' @param info [\code{set_RLum}] \code{\link{list}} (optional): info elements
setMethod("set_RLum",
          signature = signature("RLum.Data.Curve"),

          definition = function(class, recordType, curveType, data, info){

              ##check for missing curveType
              if(missing(curveType)==TRUE){

                curveType <- "NA"

              }else if (is(curveType, "character") == FALSE){

                stop("[set_RLum] Error: 'curveType' has to be of type 'character'!")

              }

              ##check for missing arguments
              if(missing(recordType) | missing(data)){

                temp.error.missing <- paste(c(

                  if(missing(recordType)){"'recordType'"}else{},
                  if(missing(data)){"'data'"}else{}),
                                            collapse=", ")

                ##set error message
                temp.error.message <- paste("[set_RLum] Missing required arguments " ,
                                       temp.error.missing,"!", sep="")
                stop(temp.error.message)
              }

              ##handle missing info argument
              if(missing(info)){

                info <- list()

              }else if (is(info, "list") == FALSE){

                stop("[set_RLum] 'info' has to be of type 'list'!")

              }

              new("RLum.Data.Curve",
                  recordType = recordType,
                  curveType = curveType,
                  data = data,
                  info = info)

            })

# constructor (get) method for object class -----------------------------------

#' @describeIn RLum.Data.Curve
#' Accessor method for RLum.Data.Curve object. The argument info.object is
#' optional to directly access the info elements. If no info element name is
#' provided, the raw curve data (matrix) will be returned.
#'
#' @param object [\code{get_RLum}] an object of class \code{\linkS4class{RLum.Data.Curve}} (\bold{required})
#' @param info.object [\code{get_RLum}] \code{\link{character}} (optional): name of the wanted info
#' element
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
