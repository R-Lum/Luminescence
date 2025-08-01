#' Class `"RLum.Data.Spectrum"`
#'
#' Class for representing luminescence spectra data (TL/OSL/RF).
#'
#' @name RLum.Data.Spectrum-class
#'
#' @docType class
#'
#' @slot recordType
#' Object of class [character] containing the type of the curve (e.g. "TL" or "OSL")
#'
#' @slot curveType
#' Object of class [character] containing curve type, allowed values are
#' "measured" or "predefined"
#'
#' @slot data
#' Object of class [matrix] containing spectrum (count) values.
#' Row labels indicate wavelength/pixel values, column labels are temperature or time values.
#'
#' @slot info
#' Object of class [list] containing further meta information objects
#'
#' @note
#' The class should only contain data for a single spectra data set. For
#' additional elements the slot `info` can be used. Objects from this class are automatically
#' created by, e.g., [read_XSYG2R]
#'
#' @section Objects from the Class:
#' Objects can be created by calls of the form `set_RLum("RLum.Data.Spectrum", ...)`.
#'
#' @section Class version: 0.5.2
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum-class], [RLum.Data-class], [plot_RLum]
#'
#' @keywords classes
#'
#' @examples
#'
#' showClass("RLum.Data.Spectrum")
#'
#' ##show example data
#' data(ExampleData.XSYG, envir = environment())
#' TL.Spectrum
#'
#' ##show data matrix
#' get_RLum(TL.Spectrum)
#'
#' ##plot spectrum
#' \dontrun{
#' plot_RLum(TL.Spectrum)
#' }
#'
#' @export
setClass(
  "RLum.Data.Spectrum",
  slots = list(
    recordType = "character",
    curveType = "character",
    data = "matrix",
    info = "list"
  ),
  contains = "RLum.Data",
  prototype = list (
    recordType = NA_character_,
    curveType = NA_character_,
    data = matrix(),
    info = list()
  )
)


## as() ---------------------------------------------------------------------
##data.frame
##COERCE RLum.Data.Spectrum >> data.frame AND data.frame >> RLum.Data.Spectrum
#' as()
#'
#' for `[RLum.Data.Spectrum-class]`
#'
#' **[RLum.Data.Spectrum-class]**
#'
#' \tabular{lll}{
#'   **from** \tab **to**\cr
#'   `data.frame` \tab `data.frame`\cr
#'   `matrix` \tab `matrix`
#'   `list` \tab `list`
#' }
#'
#' @name as
setAs("data.frame", "RLum.Data.Spectrum",
      function(from,to){

        new(to,
            recordType = NA_character_,
            curveType = NA_character_,
            data = as.matrix(from),
            info = list())
      })

setAs("RLum.Data.Spectrum", "data.frame",
      function(from){
        as.data.frame(from@data)
      })

##MATRIX
##COERCE RLum.Data.Spectrum >> matrix AND matrix >> RLum.Data.Spectrum
setAs("matrix", "RLum.Data.Spectrum",
      function(from,to){
        new(to,
            recordType = NA_character_,
            curveType = NA_character_,
            data = from,
            info = list())
      })

setAs("RLum.Data.Spectrum", "matrix",
      function(from){
        from@data
      })

## LIST
## COERCE RLum.Data.Spectrum >> list AND list >> RLum.Data.Spectrum
setAs("list", "RLum.Data.Spectrum",
      function(from, to){
        if (length(from) == 0)
          return(set_RLum("RLum.Data.Spectrum"))
        new(to,
            recordType = NA_character_,
            curveType = NA_character_,
            data = matrix(unlist(from)),
            info = list())
      })

setAs("RLum.Data.Spectrum", "list",
      function(from){
        apply(from@data, 2, list)
      })

## show() -------------------------------------------------------------------
#' @describeIn RLum.Data.Spectrum
#' Show structure of `RLum.Data.Spectrum` object
#'
#' @keywords internal
#'
#' @export
setMethod("show",
          signature(object = "RLum.Data.Spectrum"),
          function(object){

            x.range <- suppressWarnings(range(as.numeric(rownames(object@data))))
            y.range <- suppressWarnings(range(as.numeric(colnames(object@data))))
            z.range <- range(object@data)

            ##print information

            cat("\n [RLum.Data.Spectrum-class]")
            cat("\n\t recordType:", object@recordType)
            cat("\n\t curveType:",  object@curveType)
            cat("\n\t .. recorded frames:", length(object@data[1,]))
            cat("\n\t .. .. measured values per frame:", length(object@data[,1]))
            cat("\n\t .. .. range wavelength/pixel:", x.range)
            cat("\n\t .. .. range time/temp.:", y.range)
            cat("\n\t .. .. range count values:", z.range)
            cat("\n\t additional info elements:", length(object@info))
            #cat("\n\t\t >> names:", names(object@info))
            cat("\n")
          }
)


## set_RLum() ---------------------------------------------------------------
#' @describeIn RLum.Data.Spectrum
#' Construction method for RLum.Data.Spectrum object. The `info` slot is
#' optional and by default it is set to an empty list
#'
#' @param class [`set_RLum`]; [character] (*automatic*):
#' name of the `RLum` class to create.
#'
#' @param originator [character] (*automatic*):
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
#' raw curve data. If data is of type `RLum.Data.Spectrum`, this can be used
#' to re-construct the object. If the object is reconstructed, `.uid`, `.pid`
#' and `originator` are always taken from the input object
#'
#' @param info [`set_RLum`] [list]:
#' info elements
#'
#' @return
#'
#' **`[set_RLum]`**
#'
#' An object from the class `RLum.Data.Spectrum`
#'
#' @export
setMethod(
  "set_RLum",
  signature = signature("RLum.Data.Spectrum"),
  definition = function(
    class,
    originator,
    .uid,
    .pid,
    recordType = "Spectrum",
    curveType = NA_character_,
    data = matrix(),
    info = list()) {

    ##The case where an RLum.Data.Spectrum object can be provided
    ##with this RLum.Data.Spectrum objects can be provided to be reconstructed

    if (is(data, "RLum.Data.Spectrum")) {
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

      ##check for missing .uid and .pid >> this are always taken from the
      ##original dataset

      ##set empty clas form object
      newRLumDataSpectrum <- new("RLum.Data.Spectrum")

      ##fill - this is the faster way, filling in new() costs ...
      newRLumDataSpectrum@originator = data@originator
      newRLumDataSpectrum@recordType = recordType
      newRLumDataSpectrum@curveType = curveType
      newRLumDataSpectrum@data = data@data
      newRLumDataSpectrum@info = info
      newRLumDataSpectrum@.uid = data@.uid
      newRLumDataSpectrum@.pid = data@.pid

    } else {
      ##set empty class from object
      newRLumDataSpectrum <- new("RLum.Data.Spectrum")

      ##fill - this is the faster way, filling in new() costs ...
      newRLumDataSpectrum@originator = originator
      newRLumDataSpectrum@recordType = recordType
      newRLumDataSpectrum@curveType = curveType
      newRLumDataSpectrum@data = data
      newRLumDataSpectrum@info = info
      newRLumDataSpectrum@.uid = .uid
      newRLumDataSpectrum@.pid = .pid
    }

    return(newRLumDataSpectrum)

  }
)


## get_RLum() ---------------------------------------------------------------
#' @describeIn RLum.Data.Spectrum
#' Accessor method for `RLum.Data.Spectrum` object.
#'
#' @param object [`get_RLum`], [`names_RLum`] (**required**):
#' an object of class [RLum.Data.Spectrum-class]
#'
#' @param info.object [`get_RLum`]; [character] (*optional*):
#' the name of the info object to be called. If no info element name
#' is provided, the raw curve data (matrix) will be returned
#'
#' @return
#'
#' **`[get_RLum]`**
#'
#' 1. A [matrix] with the spectrum values or
#' 2. only the info object if `info.object` was set.
#'
#' @export
setMethod("get_RLum",
          signature("RLum.Data.Spectrum"),
          definition = function(object, info.object) {
              .set_function_name("get_RLum")
              on.exit(.unset_function_name(), add = TRUE)

              ##if missing info.object just show the curve values
              if (!missing(info.object)) {
                .validate_class(info.object, "character")

                if (!info.object %in% names(object@info)) {
                  .throw_error("Invalid element name, valid names are: ",
                               .collapse(names(object@info)))
                }
                unlist(object@info[info.object])
              } else {
                object@data
              }
          })


## names() ------------------------------------------------------------------
#' @describeIn RLum.Data.Spectrum
#' Returns the names info elements coming along with this curve object
#'
#' @return
#'
#' **`[names_RLum]`**
#'
#' The names of the info objects
#'
#' @export
setMethod("names_RLum",
          "RLum.Data.Spectrum",
          function(object){
            names(object@info)
          })


## bin_RLum() ---------------------------------------------------------------
#' @describeIn RLum.Data.Spectrum
#' Allows binning of RLum.Data.Spectrum data. Count values and values on the x-axis are summed-up;
#' for wavelength/energy values the mean is calculated.
#'
#' @param bin_size.col [integer] (*with default*):
#' set number of channels used for each bin, e.g. `bin_size.col = 2` means that
#' two channels are binned. Note: The function does not check the input, very large values
#' mean a full column binning (a single sum)
#'
#' @param bin_size.row [integer] (*with default*):
#' set number of channels used for each bin, e.g. `bin_size.row = 2` means that
#' two channels are binned. Note: The function does not check the input, very large values
#' mean a full row binning (a single sum)
#'
#' @return
#'
#' **`[bin_RLum.Data]`**
#'
#' Same object as input, after applying the binning.
#'
#' @export
setMethod(f = "bin_RLum.Data",
          signature = "RLum.Data.Spectrum",
          function(object, bin_size.col = 1, bin_size.row = 1) {
            .set_function_name("bin_RLum.Data.Spectrum")
            on.exit(.unset_function_name(), add = TRUE)

            ## Integrity checks ---------------------------------------------

            if (length(object@data) < 2) {
              .throw_error("'object' contains no data")
            }
            .validate_class(bin_size.row, c("numeric", "integer"))
            .validate_class(bin_size.col, c("numeric", "integer"))

            ##make sure that we do not get in trouble with negative values
            bin_size.col <- abs(bin_size.col)
            bin_size.row <- abs(bin_size.row)

            ##perform binning
            ##we want to be efficient, so we start
            ##with the larger object
            if(bin_size.row > bin_size.col){
              ##row binning first
              m <- .matrix_binning(object@data, bin_size = bin_size.row, bin_col = FALSE, names = "mean")
              m <- .matrix_binning(m, bin_size = bin_size.col, bin_col = TRUE, names = "groups")

            } else {
              ##column binning first
              m <- .matrix_binning(object@data, bin_size = bin_size.col, bin_col = TRUE, names = "groups")
              m <- .matrix_binning(m, bin_size = bin_size.row, bin_col = FALSE, names = "mean")

            }

            ##write back to object
            object@data <- m

            ##return object
            return(object)
          })
