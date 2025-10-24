#' Class `"RLum.Data.Curve"`
#'
#' Class for representing luminescence curve data.
#'
#' @name RLum.Data.Curve-class
#'
#' @docType class
#'
#' @slot recordType
#' Object of class "character" containing the type of the curve (e.g. "TL" or
#' "OSL").
#'
#' @slot curveType
#' Object of class "character" containing curve type, allowed values are
#' "measured" or "predefined".
#'
#' @slot data
#' Object of class [matrix] containing curve x and y data.
#' 'data' can also be of type `RLum.Data.Curve` to change object values without
#' de-constructing the object. For example:
#' ```
#' set_RLum(class = 'RLum.Data.Curve',
#'          data = Your.RLum.Data.Curve,
#'          recordType = 'never seen before')
#' ```
#' would just change the `recordType`. Missing arguments  the value is taken
#' from the input object in 'data' (which is already an `RLum.Data.Curve`
#' object in this example).
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
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
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
#' @export
setClass("RLum.Data.Curve",
         slots = list(
           recordType = "character",
           curveType = "character",
           data = "matrix"
           ),
         contains = "RLum.Data",
         prototype = list(
           recordType = NA_character_,
           curveType = NA_character_,
           data = matrix(data = 0, ncol = 2)
           )
         )

## as() ---------------------------------------------------------------------
##LIST
##COERCE RLum.Data.Curve >> list AND list >> RLum.Data.Curve
#' as() - RLum-object coercion
#'
#' for `[RLum.Data.Curve-class]`
#'
#' **[RLum.Data.Curve-class]**
#'
#' \tabular{lll}{
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
#' @name as
setAs("list", "RLum.Data.Curve",
      function(from,to){
        if (length(from) == 0)
          return(set_RLum("RLum.Data.Curve"))
        new(to,
            recordType = "unknown curve type",
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
                  recordType = "unknown curve type",
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
            recordType = "unknown curve type",
            curveType = NA_character_,
            data = from,
            info = list())
      })

setAs("RLum.Data.Curve", "matrix",
      function(from){
        from@data
      })

## show() -------------------------------------------------------------------
#' @describeIn show
#' Show the structure of `RLum.Data.Curve` objects.
#'
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

## set_RLum() ---------------------------------------------------------------
#' @describeIn set_RLum
#' Construction method for [RLum.Data.Curve-class] objects.
#'
#' @param recordType [character] (*optional*):
#' record type (e.g., "OSL")
#'
#' @param curveType [character] (*optional*):
#' curve type (e.g., "predefined" or "measured")
#'
#' @param data [matrix] or [list] (*with default*):
#' a matrix containing raw curve data or a list containing the data to be
#' stored in the object (for [RLum.Results-class] objects) . If `data` itself
#' is a `RLum.Data.Curve`-object this can be used to re-construct the object,
#' i.e. modified parameters except `.uid`, `.pid` and `originator`. The rest
#' will be subject to copy and paste unless provided.
#'
#' @param info [list] (*optional*):
#' a list containing additional info data for the object.
#'
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
    if (inherits(data, "RLum.Data.Curve")) {
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

## get_RLum() ---------------------------------------------------------------
#' @describeIn get_RLum
#' Accessor method for [RLum.Data.Curve-class] object.
#' The argument `info.object` is optional to directly access the info elements.
#' If no info element name is provided, the raw curve data (matrix) will be
#' returned.
#'
#' @export
setMethod("get_RLum",
          signature("RLum.Data.Curve"),
          definition = function(object, info.object = NULL) {
          .set_function_name("get_RLum")
          on.exit(.unset_function_name(), add = TRUE)

    if (is.null(info.object)) {
      return(object@data)
    }
    if (length(object@info) == 0) {
      .throw_warning("'object' has no info objects, NULL returned")
      return(NULL)
    }
    if (!info.object %in% names(object@info)) {
      .throw_warning("Invalid 'info.object' name, valid names are: ",
                     .collapse(names(object@info)))
      return(NULL)
    }

    unlist(object@info[info.object])
  })


## length_RLum() ------------------------------------------------------------
#' @describeIn length_RLum
#' Returns the number of channels in the curve, which is the maximum of the
#' value time/temperature of the curve (corresponding to the stimulation
#' length).
#'
#' @export
setMethod("length_RLum",
          "RLum.Data.Curve",
          function(object){
            max(object@data[,1])
          })


## names_RLum() -------------------------------------------------------------
#' @describeIn names_RLum
#' Returns the names info elements stored in the object.
#'
#' @export
setMethod("names_RLum",
          "RLum.Data.Curve",
          function(object){
            names(object@info)
          })


## bin_RLum.Data() ----------------------------------------------------------
#' @describeIn bin_RLum.Data
#' Allows binning of RLum.Data.Curve data.
#'
#' @param bin_size [integer] (*with default*):
#' number of channels used for each bin, e.g. `bin_size = 2` means that
#' two channels are binned.
#'
#' @export
setMethod(f = "bin_RLum.Data",
          signature = "RLum.Data.Curve",
          function(object, bin_size = 2) {
            .set_function_name("bin_RLum.Data.Curve")
            on.exit(.unset_function_name(), add = TRUE)

            .validate_positive_scalar(bin_size, int = TRUE)

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
          })


## smooth_RLum() ------------------------------------------------------------
#' @describeIn smooth_RLum
#' Smoothing of [RLum.Data.Curve-class] objects using a rolling mean or median.
#' For methods `"mean"` and `"median"`, smoothing is performed by rolling
#' mean and rolling median with window of size `k`. Method `"Carter_etal_2018"`
#' implements a Poisson smoother for dark-background signals measured by a
#' photomultiplier tube.
#'
#' @param k [`smooth_RLum`]; [integer] (*with default*):
#' window for the rolling mean or median. If `NULL`, this set automatically
#' (ignored if `method = "Carter_etal_2018"`).
#'
#' @param fill [`smooth_RLum`]; [numeric] (*with default*):
#' value used to pad the result so to have the same length as the input.
#'
#' @param align [`smooth_RLum`]; [character] (*with default*):
#' one of `"right"`, `"center"` or `"left"`, specifying whether the index
#' of the result should be right-aligned (default), centred, or left-aligned
#' compared to the rolling window of observations (ignored if
#' `method = "Carter_etal_2018"`).
#'
#' @param method [`smooth_RLum`]; [character] (*with default*):
#' smoothing method to be applied: one of `"mean"`, `"median"` or
#' `"Carter_etal_2018"`.
#'
#' @param p_acceptance [`smooth_RLum`]; [numeric] (*with default*):
#' probability threshold of accepting a value to be a sample from a Poisson
#' distribution (only used for `method = "Carter_etal_2018"`). Values that
#' have a Poisson probability below the threshold are replaced by the average
#' over the four neighbouring values.
#'
#' @references
#' Carter, J., Cresswell, A.J., Kinnaird, T.C., Carmichael, L.A., Murphy, S. &
#' Sanderson, D.C.W., 2018. Non-Poisson variations in photomultipliers and
#' implications for luminescence dating. Radiation Measurements 120, 267-273.
#' \doi{10.1016/j.radmeas.2018.05.010}
#'
#' @export
setMethod(
  f = "smooth_RLum",
  signature = "RLum.Data.Curve",
  function(object, k = NULL, fill = NA, align = "right", method = "mean",
           p_acceptance = 1e-7) {

        object@data[,2] <- .smoothing(
          x = object@data[,2],
          k = k,
          fill = fill,
          align = align,
          p_acceptance = p_acceptance,
          method = method)

        ##return via set function to get a new id
        set_RLum(class = "RLum.Data.Curve",
                 originator = "smooth_RLum",
                 data = object)
    }
 )

## melt_RLum() --------------------------------------------------------------
#' @describeIn melt_RLum
#' Melts [RLum.Data.Curve-class] objects into a flat data.frame with columns
#' `X`, `Y`, `TYPE`, `UID`, to be used in combination with other packages
#' such as `ggplot2`.
#'
#' @export
setMethod(
  f = "melt_RLum",
  signature = "RLum.Data.Curve",
  function(object) {
    data.frame(
      X = object@data[,1],
      Y = object@data[,2],
      TYPE = object@recordType,
      UID = object@.uid)
  }
)
