#' @include get_RLum.R set_RLum.R names_RLum.R length_RLum.R bin_RLum.Data.R smooth_RLum.R
NULL

#' Class \code{"RLum.Data.Curve"}
#'
#' Class for representing luminescence curve data.
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
#' For example: \code{set_RLum(class = 'RLum.Data.Curve',
#' data = Your.RLum.Data.Curve, recordType = 'never seen before')}
#' would just change the recordType. Missing arguements  the value is taken from the input object
#' in 'data' (which is already an RLum.Data.Curve object in this example)
#'
#'
#' @note The class should only contain data for a single curve. For additional
#' elements the slot \code{info} can be used (e.g. providing additional heating
#' ramp curve). Objects from the class \code{RLum.Data.Curve} are produced by other
#' functions (partyl within \code{\linkS4class{RLum.Analysis}} objects),
#' namely: \code{\link{Risoe.BINfileData2RLum.Analysis}}, \code{\link{read_XSYG2R}}
#'
#' @section Create objects from this Class: Objects can be created by calls of the form
#' \code{set_RLum(class = "RLum.Data.Curve", ...)}.
#'
#' @section Class version: 0.5.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @seealso \code{\linkS4class{RLum}}, \code{\linkS4class{RLum.Data}},
#' \code{\link{plot_RLum}}, \code{\link{merge_RLum}}
#'
#' @keywords classes
#'
#' @examples
#'
#' showClass("RLum.Data.Curve")
#'
#' ##set empty curve object
#' set_RLum(class = "RLum.Data.Curve")
#'
#' @export
setClass("RLum.Data.Curve",
         slots = list(
           recordType = "character",
           curveType = "character",
           data = "matrix"
           ),
         contains = "RLum.Data",
         prototype = list (
           recordType = NA_character_,
           curveType = NA_character_,
           data = matrix(data = 0, ncol = 2)
           )
         )

####################################################################################################
###as()
####################################################################################################
##LIST
##COERCE RLum.Data.Curve >> list AND list >> RLum.Data.Curve
#' as() - RLum-object coercion
#'
#' for \code{[RLum.Data.Curve]}
#'
#' \bold{[RLum.Data.Curve]}\cr
#'
#' \tabular{ll}{
#'  \bold{from} \tab \bold{to}\cr
#'   \code{list} \tab \code{list} \cr
#'   \code{data.frame} \tab \code{data.frame}\cr
#'   \code{matrix} \tab \code{matrix}
#'
#' }
#'
#' @param from \code{\linkS4class{RLum}} or \code{\link{list}}, \code{\link{data.frame}}, \code{\link{matrix}}
#' (\bold{required}): object to be coerced from
#'
#' @param to \code{\link{character}} (\bold{required}): class name to be coerced to
#'
#' @seealso \code{\link[methods]{as}}
#'
#' @note Due to the complex structure of the \code{RLum} objects itself a coercing to standard
#' R data structures will be always loosely!
#'
#' @name as
#'
setAs("list", "RLum.Data.Curve",
      function(from,to){

        new(to,
            recordType = "unkown curve type",
            curveType = NA_character_,
            data = matrix(unlist(from), ncol = 2),
            info = list())
      })


setAs("RLum.Data.Curve", "list",
      function(from){

          list(x = from@data[,1], y = from@data[,2])

      })

##DATA.FRAME
##COERCE RLum.Data.Curve >> data.frame AND data.frame >> RLum.Data.Curve
setAs("data.frame", "RLum.Data.Curve",
      function(from,to){

              new(to,
                  recordType = "unkown curve type",
                  curveType = NA_character_,
                  data = as.matrix(from),
                  info = list())
            })

setAs("RLum.Data.Curve", "data.frame",
      function(from){

        data.frame(x = from@data[,1],
                   y = from@data[,2])

      })


##MATRIX
##COERCE RLum.Data.Curve >> matrix AND matrix >> RLum.Data.Curve
setAs("matrix", "RLum.Data.Curve",
      function(from,to){

        new(to,
            recordType = "unkown curve type",
            curveType = NA_character_,
            data = from,
            info = list())

      })


setAs("RLum.Data.Curve", "matrix",
      function(from){
        from@data

      })


####################################################################################################
###show()
####################################################################################################
#' @describeIn RLum.Data.Curve
#' Show structure of \code{RLum.Data.Curve} object
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
            cat("\n\t .. range of y-values:",
                suppressWarnings(min(object@data[,2], na.rm = TRUE)),
                suppressWarnings(max(object@data[,2], na.rm = TRUE)),
                if(anyNA(object@data[,2])){"(contains NA values)"}else{""}
               )
            cat("\n\t additional info elements:", length(object@info))
            #cat("\n\t\t >> names:", names(object@info))
          }
)




####################################################################################################
###set_RLum()
####################################################################################################
#' @describeIn RLum.Data.Curve
#' Construction method for RLum.Data.Curve object. The slot info is optional
#' and predefined as empty list by default.
#'
#' @param class [\code{set_RLum}] \code{\link{character}} (\bold{required}): name of the \code{RLum} class to create
#' @param originator [\code{set_RLum}] \code{\link{character}} (automatic): contains the name of the calling function
#' (the function that produces this object); can be set manually.
#' @param .uid [\code{set_RLum}] \code{\link{character}} (automatic): sets an unique ID for this object
#' using the internal C++ function \code{.create_UID}.
#' @param .pid [\code{set_RLum}] \code{\link{character}} (with default): option to provide a parent id for nesting
#' at will.
#' @param recordType [\code{set_RLum}] \code{\link{character}} (optional): record type (e.g., "OSL")
#' @param curveType [\code{set_RLum}] \code{\link{character}} (optional): curve type (e.g., "predefined" or "measured")
#' @param data [\code{set_RLum}] \code{\link{matrix}} (\bold{required}): raw curve data.
#' If \code{data} itself is a \code{RLum.Data.Curve}-object this can be used to re-construct the object
#' (s. Details)
#' @param info [\code{set_RLum}] \code{\link{list}} (optional): info elements
#'
#' @return
#'
#' \bold{\code{set_RLum}}\cr
#'
#' Returns an \code{\linkS4class{RLum.Data.Curve}} object.
#'
#' @export
setMethod(
  "set_RLum",
  signature = signature("RLum.Data.Curve"),

  definition = function(class,
                        originator,
                        .uid,
                        .pid,
                        recordType = NA_character_,
                        curveType = NA_character_,
                        data = matrix(0, ncol = 2),
                        info = list()) {

    ##The case where an RLum.Data.Curve object can be provided
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

      ##check for missing .uid
      if(missing(.uid)){
        .uid <- data@.uid

      }

      ##check for missing .pid
      if(missing(.pid)){
        .pid <- data@.pid

      }

      ##check for missing originator
      if(missing(originator)){
        originator <- data@originator

      }

      ##set empty class from object
      newRLumDataCurve <- new("RLum.Data.Curve")

      ##fill - this is the faster way, filling in new() costs ...
      newRLumDataCurve@recordType <- recordType
      newRLumDataCurve@curveType <- curveType
      newRLumDataCurve@data <- data@data
      newRLumDataCurve@info <- info
      newRLumDataCurve@originator <- originator
      newRLumDataCurve@.uid <- .uid
      newRLumDataCurve@.pid <- .pid

      return(newRLumDataCurve)

    }else{

      ##set empty class form object
      newRLumDataCurve <- new("RLum.Data.Curve")

      ##fill - this is the faster way, filling in new() costs ...
      newRLumDataCurve@originator <- originator
      newRLumDataCurve@recordType <- recordType
      newRLumDataCurve@curveType <- curveType
      newRLumDataCurve@data <- data
      newRLumDataCurve@info <- info
      newRLumDataCurve@.uid <- .uid
      newRLumDataCurve@.pid <- .pid

      return(newRLumDataCurve)

    }

  }
)

####################################################################################################
###get_RLum()
####################################################################################################
#' @describeIn RLum.Data.Curve
#' Accessor method for RLum.Data.Curve object. The argument info.object is
#' optional to directly access the info elements. If no info element name is
#' provided, the raw curve data (matrix) will be returned.
#'
#' @param object [\code{show_RLum}][\code{get_RLum}][\code{length_RLum}][\code{names_RLum}] an object of
#' class \code{\linkS4class{RLum.Data.Curve}} (\bold{required})
#' @param info.object [\code{get_RLum}] \code{\link{character}} (optional): name of the wanted info
#' element
#'
#' @return
#'
#' \bold{\code{get_RLum}}\cr
#'
#' (1) A \code{\link{matrix}} with the curve values or \cr
#' (2) only the info object if \code{info.object} was set.\cr
#'
#' @export
setMethod("get_RLum",
          signature("RLum.Data.Curve"),
          definition = function(object, info.object = NULL) {

           ##Check if function is of type RLum.Data.Curve
           if(is(object, "RLum.Data.Curve") == FALSE){

              stop("[get_RLum] Function valid for 'RLum.Data.Curve' objects only!")

           }

           ##if info.object == NULL just show the curve values
          if(!is.null(info.object)) {

              if(info.object %in% names(object@info)){

                unlist(object@info[info.object])

              }else{

                ##check for entries
                if(length(object@info) == 0){

                  warning("[get_RLum] This RLum.Data.Curve object has no info objects! NULL returned!)")
                  return(NULL)

                }else{

                  ##grep names
                  temp.element.names <- paste(names(object@info), collapse = ", ")

                  warning.text <- paste("[get_RLum] Invalid info.object name. Valid names are:", temp.element.names)

                  warning(warning.text, call. = FALSE)
                  return(NULL)

                }

              }


             }else{

                    object@data

             }
          })

####################################################################################################
###length_RLum()
####################################################################################################
#' @describeIn RLum.Data.Curve
#' Returns the length of the curve object, which is the maximum of the
#' value time/temperature of the curve (corresponding to the stimulation length)
#'
#' @return
#' \bold{\code{length_RLum}}\cr
#'
#' Number of channels in the curve (row number of the matrix)
#'
#' @export
setMethod("length_RLum",
          "RLum.Data.Curve",
          function(object){
            max(object@data[,1])

          })

####################################################################################################
###names_RLum()
####################################################################################################
#' @describeIn RLum.Data.Curve
#' Returns the names info elements coming along with this curve object
#'
#' @return
#'
#' \bold{\code{names_RLum}}\cr
#'
#' Names of the info elements (slot \code{info})
#'
#' @export
setMethod("names_RLum",
          "RLum.Data.Curve",
          function(object){
            names(object@info)

          })

####################################################################################################
###bin_RLum.Data()
####################################################################################################
#' @describeIn RLum.Data.Curve
#' Allows binning of specific objects
#'
#' @param bin_size [\code{bin_RLum}] \code{\link{integer}} (with default): set number of channels
#' used for each bin, e.g. \code{bin_size = 2} means that two channels are binned.
#'
#' @return
#'
#' \bold{\code{bin_RLum.Data}}\cr
#'
#' Same object as input, after applying the binning.
#'
#' @export
setMethod(f = "bin_RLum.Data",
          signature = "RLum.Data.Curve",
          function(object, bin_size = 2) {

            ##check for invalid bin_size values
            if (!is.null(bin_size) && bin_size > 0) {
              ##set stepping vector
              stepping <- seq(1, nrow(object@data), by = bin_size)

              ##get bin vector
              bin_vector <- object@data[, 2]

              ##set desired length of the vector
              ##to avoid add effects later
              length(bin_vector) <-
                suppressWarnings(prod(dim(matrix(
                  bin_vector, ncol = length(stepping)
                ))))

              ##define new matrix for binning
              bin_matrix <-
                matrix(bin_vector, ncol = length(stepping))

              ##calcuate column sums and replace matrix
              ##this is much faster than anly apply loop
              object@data <-
                matrix(c(object@data[stepping], colSums(bin_matrix, na.rm = TRUE)), ncol = 2)

              ##set matrix
              return(set_RLum(class = "RLum.Data.Curve",
                              data = object))
            } else{
              warning("Argument 'bin_size' invald, nothing was done!")

              ##just return the object
              return(object)

            }

          })

####################################################################################################
###smooth_RLum()
####################################################################################################
#' @describeIn RLum.Data.Curve
#' Smoothing of RLum.Data.Curve objects using the function \code{\link[zoo]{rollmean}} or \code{\link[zoo]{rollmedian}}.
#' In particular the internal function \code{.smoothing} is used.
#'
#' @param k [\code{smooth_RLum}] \code{\link{integer}} (with default): window for the rolling mean; must be odd for rollmedian.
#' If nothing is set k is set automatically
#'
#' @param fill [\code{smooth_RLum}] \code{\link{numeric}} (with default): a vector defining the left and the right hand data
#'
#' @param align [\code{smooth_RLum}] \code{\link{character}} (with default): specifying whether the index of the result should be
#' left- or right-aligned or centered (default) compared to the rolling window of observations, allowed
#' \code{"right"}, \code{"center"} and \code{left}
#'
#' @param method [\code{smooth_RLum}] \code{\link{character}} (with default): defines which method should be applied for the
#' smoothing: \code{"mean"} or \code{"median"}
#'
#' @return
#'
#' \bold{\code{smooth_RLum}}\cr
#'
#' Same object as input, after smoothing
#'
#' @export
setMethod(
  f = "smooth_RLum",
  signature = "RLum.Data.Curve",
    function(object, k = NULL, fill = NA, align = "right", method = "mean") {

        object@data[,2] <- .smoothing(
          x = object@data[,2],
          k = k,
          fill = fill,
          align = align,
          method = method)

        ##return via set function to get a new id
        set_RLum(class = "RLum.Data.Curve",
                 originator = "smooth_RLum",
                 data = object)

    }
 )

