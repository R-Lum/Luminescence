#' @include get_RLum.R set_RLum.R names_RLum.R length_RLum.R bin_RLum.Data.R smooth_RLum.R
NULL

#' Class `"RLum.Data.Curve"`
#'
#' Class for representing luminescence curve data.
#'
#' @name RLum.Data.Curve-class
#'
#' @docType class
#'
#' @slot recordType
#' Object of class "character" containing the type of the curve (e.g. "TL" or "OSL")
#'
#' @slot curveType
#' Object of class "character" containing curve type, allowed values are measured or predefined
#'
#' @slot data
#' Object of class [matrix] containing curve x and y data.
#' 'data' can also be of type `RLum.Data.Curve` to change object values without
#' deconstructing the object. For example:
#' ```
#' set_RLum(class = 'RLum.Data.Curve',
#'          data = Your.RLum.Data.Curve,
#'          recordType = 'never seen before')
#' ```
#' would just change the `recordType`. Missing arguments  the value is taken
#' from the input object in 'data' (which is already an RLum.Data.Curve object
#' in this example)
#'
#'
#' @note
#' The class should only contain data for a single curve. For additional
#' elements the slot `info` can be used (e.g. providing additional heating
#' ramp curve). Objects from the class `RLum.Data.Curve` are produced by other
#' functions (partly within [RLum.Analysis-class] objects),
#' namely: [Risoe.BINfileData2RLum.Analysis], [read_XSYG2R]
#'
#' @section Create objects from this Class:
#' Objects can be created by calls of the form
#' `set_RLum(class = "RLum.Data.Curve", ...)`.
#'
#' @section Class version: 0.5.1
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS - Université Bordeaux Montaigne (France)
#'
#' @seealso [RLum-class], [RLum.Data-class], [plot_RLum], [merge_RLum]
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
#' @md
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

# as() ----------------------------------------------------------------------------------------
##LIST
##COERCE RLum.Data.Curve >> list AND list >> RLum.Data.Curve
#' as() - RLum-object coercion
#'
#' for `[RLum.Data.Curve-class]`
#'
#' **[RLum.Data.Curve-class]**
#'
#' \tabular{ll}{
#'  **from** \tab **to**\cr
#'   `list` \tab `list` \cr
#'   `data.frame` \tab `data.frame`\cr
#'   `matrix` \tab `matrix`
#' }
#'
#' @param from [RLum-class], [list], [data.frame], [matrix] (**required**):
#'  object to be coerced from
#'
#' @param to [character] (**required**):
#' class name to be coerced to
#'
#' @seealso [methods::as]
#'
#' @note
#' Due to the complex structure of the `RLum` objects itself a coercing to standard
#' R data structures will be always loosely!
#'
#' @md
#' @name as
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

# show() --------------------------------------------------------------------------------------
#' @describeIn RLum.Data.Curve
#' Show structure of `RLum.Data.Curve` object
#'
#' @keywords internal
#'
#' @md
#' @export
setMethod("show",
          signature(object = "RLum.Data.Curve"),
          function(object){

            ##print information
            cat("\n [RLum.Data.Curve-class]")
            cat("\n\t recordType:", object@recordType)
            cat("\n\t curveType:",  object@curveType)
            cat("\n\t measured values:", length(object@data[,1]))
            cat("\n\t .. range of x-values:", suppressWarnings(range(object@data[,1])))
            cat("\n\t .. range of y-values:",
                suppressWarnings(min(object@data[,2], na.rm = TRUE)),
                suppressWarnings(max(object@data[,2], na.rm = TRUE)),
                if(anyNA(object@data[,2])){"(contains NA values)"}else{""}
               )
            cat("\n\t additional info elements:", length(object@info), "\n")
            #cat("\n\t\t >> names:", names(object@info))
          }
)



# set_RLum() ----------------------------------------------------------------------------------
#' @describeIn RLum.Data.Curve
#' Construction method for RLum.Data.Curve object. The slot info is optional
#' and predefined as empty list by default.
#'
#' @param class [`set_RLum`]; [character] (**required**):
#' name of the `RLum` class to create
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
#' @param recordType [`set_RLum`]; [character] (*optional*):
#' record type (e.g., "OSL")
#'
#' @param curveType [`set_RLum`]; [character] (*optional*):
#' curve type (e.g., "predefined" or "measured")
#'
#' @param data [`set_RLum`]; [matrix] (**required**):
#' raw curve data. If `data` itself is a `RLum.Data.Curve`-object this can be
#' used to re-construct the object (s. details), i.e. modified parameters except
#' `.uid`, `.pid` and `originator`. The rest will be subject to copy and paste unless provided.
#'
#' @param info [`set_RLum`]; [list] (*optional*):
#' info elements
#'
#' @return
#'
#' **`set_RLum`**
#'
#' Returns an [RLum.Data.Curve-class] object.
#'
#' @md
#' @export
setMethod(
  "set_RLum",
  signature = signature("RLum.Data.Curve"),
  definition = function(
    class,
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
      if (missing(curveType))
        curveType <- data@curveType

      ##check for missing recordType
      if(missing(recordType))
        recordType <- data@recordType

      ##check for missing data ... not possible as data is the object itself

      ##check for missing info
      if(missing(info))
        info <- data@info

      ##check for missing .uid and .pid and originator
      ##>> no this is always taken from the old object here

      ##set empty class from object
      newRLumDataCurve <- new("RLum.Data.Curve")

      ##fill - this is the faster way, filling in new() costs ...
      newRLumDataCurve@recordType <- recordType
      newRLumDataCurve@curveType <- curveType
      newRLumDataCurve@data <- data@data
      newRLumDataCurve@info <- info
      newRLumDataCurve@originator <- data@originator
      newRLumDataCurve@.uid <- data@.uid
      newRLumDataCurve@.pid <- data@.pid

    } else {

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

    }
    return(newRLumDataCurve)
  }
)

# get_RLum() ----------------------------------------------------------------------------------
#' @describeIn RLum.Data.Curve
#' Accessor method for RLum.Data.Curve object. The argument info.object is
#' optional to directly access the info elements. If no info element name is
#' provided, the raw curve data (matrix) will be returned.
#'
#' @param object [`get_RLum`], [`length_RLum`], [`names_RLum`] (**required**):
#' an object of class [RLum.Data.Curve-class]
#'
#' @param info.object [`get_RLum`] [character] (*optional*):
#' name of the wanted info element
#'
#' @return
#'
#' **`get_RLum`**
#'
#' 1. A [matrix] with the curve values or
#' 2. only the info object if `info.object` was set.
#'
#' @md
#' @export
setMethod("get_RLum",
          signature("RLum.Data.Curve"),
          definition = function(object, info.object = NULL) {

           ##if info.object == NULL just show the curve values
           if(!is.null(info.object)) {
              if(info.object %in% names(object@info)){
                unlist(object@info[info.object])

              }else{
                ##check for entries
                if(length(object@info) == 0){
                  warning("[get_RLum()] This RLum.Data.Curve object has no info objects! NULL returned!)")
                  return(NULL)

                }else{
                  ##grep names
                  temp.element.names <- paste(names(object@info), collapse = ", ")

                  warning.text <- paste("[get_RLum()] Invalid info.object name. Valid names are:", temp.element.names)

                   warning(warning.text, call. = FALSE)
                  return(NULL)

                }

              }

             }else{
                object@data

             }
          })


# length_RLum() -------------------------------------------------------------------------------
#' @describeIn RLum.Data.Curve
#' Returns the length of the curve object, which is the maximum of the
#' value time/temperature of the curve (corresponding to the stimulation length)
#'
#' @return
#'
#' **`length_RLum`**
#'
#' Number of channels in the curve (row number of the matrix)
#'
#' @md
#' @export
setMethod("length_RLum",
          "RLum.Data.Curve",
          function(object){
            max(object@data[,1])

          })


# names_RLum() --------------------------------------------------------------------------------
#' @describeIn RLum.Data.Curve
#' Returns the names info elements coming along with this curve object
#'
#' @return
#'
#' **`names_RLum`**
#'
#' Names of the info elements (slot `info`)
#'
#' @export
setMethod("names_RLum",
          "RLum.Data.Curve",
          function(object){
            names(object@info)

          })


# bin_RLum.Data() -----------------------------------------------------------------------------
#' @describeIn RLum.Data.Curve
#' Allows binning of specific objects
#'
#' @param bin_size [integer] (*with default*):
#' set number of channels used for each bin, e.g. `bin_size = 2` means that
#' two channels are binned.
#'
#' @return
#'
#' **`bin_RLum.Data`**
#'
#' Same object as input, after applying the binning.
#'
#' @md
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
              warning("Argument 'bin_size' invalid, nothing was done!")

              ##just return the object
              return(object)

            }

          })


# smooth_RLum() -------------------------------------------------------------------------------
#' @describeIn RLum.Data.Curve
#' Smoothing of RLum.Data.Curve objects using the function [zoo::rollmean] or [zoo::rollmedian][zoo::rollmean].
#' In particular the internal function `.smoothing` is used.
#'
#' @param k [`smooth_RLum`]; [integer] (*with default*):
#' window for the rolling mean; must be odd for `rollmedian`.
#' If nothing is set k is set automatically
#'
#' @param fill [`smooth_RLum`]; [numeric] (*with default*):
#' a vector defining the left and the right hand data
#'
#' @param align [`smooth_RLum`]; [character] (*with default*):
#' specifying whether the index of the result should be left- or right-aligned
#' or centred (default) compared to the rolling window of observations, allowed
#' `"right"`, `"center"` and `"left"`
#'
#' @param method [`smooth_RLum`]; [character] (*with default*):
#' defines which method should be applied for the smoothing: `"mean"` or `"median"`
#'
#' @return
#'
#' **`smooth_RLum`**
#'
#' Same object as input, after smoothing
#'
#' @md
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

