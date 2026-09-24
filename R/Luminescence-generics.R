## bin_RLum.Data ------------------------------------------------------------
#' @title Channel binning for RLum.Data-class objects
#'
#' @description
#' The function provides a generalised access point for specific
#' [Luminescence::RLum.Data-class] objects. Depending on the input object, the corresponding
#' function will be selected.
#'
#' @param object [Luminescence::RLum.Data-class] (**required**):
#' S4 object of class `RLum.Data`
#'
#' @param ... further arguments passed to the specific class method
#'
#' @return
#' An object of the same type as the input provided after binning is applied.
#'
#' @section Function version: 0.2.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @note Currently only `RLum.Data` objects of class [Luminescence::RLum.Data.Curve-class]
#' and [Luminescence::RLum.Data.Spectrum-class] are supported.
#'
#' @seealso [Luminescence::RLum.Data.Curve-class], [Luminescence::RLum.Data.Spectrum-class]
#'
#' @examples
#'
#' ## load example data
#' data(ExampleData.CW_OSL_Curve, envir = environment())
#'
#' ## create RLum.Data.Curve object from this example
#' curve <-
#'   set_RLum(
#'       class = "RLum.Data.Curve",
#'       recordType = "OSL",
#'       data = as.matrix(ExampleData.CW_OSL_Curve)
#'   )
#'
#' ## plot data without and with 2 and 4 channel binning
#' plot_RLum(curve)
#' plot_RLum(bin_RLum.Data(curve, bin_size = 2))
#' plot_RLum(bin_RLum.Data(curve, bin_size = 4))
#'
#' @keywords utilities
#'
#' @export
setGeneric("bin_RLum.Data", function(object, ...)
  standardGeneric("bin_RLum.Data")
)


## get_RLum() ---------------------------------------------------------------
#' @title General accessor function for RLum-class objects
#'
#' @description
#' The function provides a generalised access point for specific
#' [Luminescence::RLum-class] objects. Depending on the input object, the corresponding
#' function will be selected.
#'
#' @param object [Luminescence::RLum-class] (**required**):
#' S4 object of class `RLum` or an object of type [list] containing only objects
#' of type [Luminescence::RLum-class]
#'
#' @param ... further arguments passed to the specific class method.
#'
#' @return
#' An object of the same type as the input object provided.
#'
#' @section Function version: 0.3.3
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @seealso [Luminescence::RLum.Data.Curve-class], [Luminescence::RLum.Data.Image-class],
#' [Luminescence::RLum.Data.Spectrum-class], [Luminescence::RLum.Analysis-class],
#' [Luminescence::RLum.Results-class]
#'
#' @keywords utilities
#'
#' @examples
#'
#' ## Example based using data and from the calc_CentralDose() function
#'
#' ## load example data
#' data(ExampleData.DeValues, envir = environment())
#'
#' ## apply the central dose model 1st time
#' temp1 <- calc_CentralDose(ExampleData.DeValues$CA1)
#'
#' ## get results and store them in a new object
#' temp.get <- get_RLum(object = temp1)
#'
#' @export
setGeneric("get_RLum", function(object, ...)
  standardGeneric("get_RLum")
)

#' @describeIn get_RLum
#' Returns a list of [Luminescence::RLum-class] objects that had been passed to
#' [Luminescence::get_RLum]
#'
#' @param class [character] (*optional*):
#' define which class gets selected if applied to a list, e.g., if a list
#' consists of different type of [Luminescence::RLum-class] objects, this arguments allows
#' to make a selection. If nothing is provided, all [Luminescence::RLum-class] objects are
#' treated.
#'
#' @param null.rm [logical] (*with default*):
#' whether empty and `NULL` objects should be removed.
#'
#' @export
setMethod("get_RLum", signature = "list",
    function(object, class = NULL, null.rm = FALSE, ...) {
      ## input validation
      .validate_class(class, "character", null.ok = TRUE, length = 1)
      if (!is.null(class) && is.na(class))
        .throw_error("'class' cannot contain missing values")
      .validate_logical_scalar(null.rm)

      ## take care of the class argument
      if (!is.null(class)) {
        sel <- class[1] == vapply(object, function(x) class(x)[1], character(1))
        if (any(sel))
          object <- object[sel]
      }

      ## make remove all non-RLum objects
      selection <- lapply(seq_along(object), function(x) {
        ## get rid of all objects that are not of type RLum, this is better
        ## than leaving that to the user
        if (!inherits(object[[x]], what = "RLum")) {
          warning("[get_RLum()] object #", x, " in the list is not of class ",
                  "'RLum' and has been removed", call. = FALSE)
          return(NULL)
        }

          ## it might be the case the object already comes with empty objects,
          ## this would cause a crash
          if (inherits(object[[x]], "RLum.Analysis") &&
              length(object[[x]]@records) == 0)
            return(NULL)

          get_RLum(object[[x]], ...)
      })

      ## remove empty or NULL objects after the selection ... if wanted
      if (null.rm) {
        ## first set all empty objects to NULL ... for RLum.Analysis objects
        selection <- lapply(selection, function(x) {
          if (length(x) == 0 ||
              (inherits(x, "RLum.Analysis") && length(x@records) == 0))
            return(NULL)
          return(x)
        })
        ## get rid of all NULL objects
        selection <- selection[!vapply(selection, is.null, logical(1))]
      }
      return(selection)
    })

#' @describeIn get_RLum
#' Returns `NULL`.
#'
#' @export
setMethod("get_RLum", signature = "NULL",
    function(object, ...) {
      NULL
    })


## remove_RLum() ------------------------------------------------------------
#' @title Strips records from RLum-class objects
#'
#' @description
#' Remove records from an RLum-class object in a convenient way using
#' [Luminescence::get_RLum] for the selection.
#'
#' @param object [Luminescence::RLum-class] (**required**):
#' object with records to be removed.
#'
#' @param ... further arguments passed to the specific class method
#'
#' @return
#' An object of the same type as the input provided with records removed; it
#' can result in an empty object.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @seealso [Luminescence::RLum.Analysis-class]
#'
#' @examples
#' ## load example data
#' data(ExampleData.XSYG, envir = environment())
#' sar <- OSL.SARMeasurement$Sequence.Object[1:9]
#'
#' ## strop only OSL curves
#' sar <- remove_RLum(sar, recordType = "OSL")
#' sar
#'
#' @keywords utilities
#'
#' @export
setGeneric("remove_RLum", function(object, ...)
  standardGeneric("remove_RLum")
)

#' @describeIn remove_RLum
#' Returns a list of [Luminescence::RLum-class] objects where the selected records are stripped
#'
#' @export
setMethod("remove_RLum", signature = "list",
          function(object, ...) {
            ## apply method in the objects and return the same
            tmp <- lapply(object, function(x) {
              if (inherits(x, "RLum.Analysis")) {
                return(remove_RLum(x,...))
              } else {
                return(x)
              }
            })

            ## remove empty elements
            tmp[lengths(tmp) > 0]
})


## hist() -------------------------------------------------------------------
#' @title Simple histograms of RLum-class objects
#'
#' @param x [Luminescence::RLum-class] (**required**):
#' S4 object of class `RLum`.
#'
#' @param ... further arguments passed to [graphics::hist] (or to the specific
#' class methods).
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)\cr
#'
#' @seealso [Luminescence::plot_Histogram]
#'
#' @keywords utilities
#' @name hist
NULL


## length_RLum() ------------------------------------------------------------
#' @title Length of RLum-class and Risoe-class objects
#'
#' @param x [Luminescence::RLum-class] or [Luminescence::Risoe.BINfileData-class] (**required**):
#' S4 object.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)\cr
#'
#' @keywords utilities
#' @name length
NULL


## melt_RLum() --------------------------------------------------------------
#' @title Melt RLum-class objects into a flat data.frame
#'
#' @param object [Luminescence::RLum-class] (**required**):
#' S4 object of class `RLum`
#'
#' @param ... further arguments passed to the specific class method
#'
#' @return
#' A flat [data.frame].
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @seealso [Luminescence::RLum.Data.Curve-class], [Luminescence::RLum.Analysis-class]
#'
#' @examples
#' data(ExampleData.XSYG, envir = environment())
#' melt_RLum(OSL.SARMeasurement[[2]][[1]])
#'
#' @keywords utilities
#'
#' @export
setGeneric("melt_RLum", function(object, ...)
  standardGeneric("melt_RLum")
)

#' @describeIn melt_RLum
#' Returns a list of melted [Luminescence::RLum-class] objects; non-RLum objects are silently
#' removed.
#'
#' @export
setMethod("melt_RLum", signature = "list",
    function(object, ...) {
      ## silently remove non-RLum objects
      l <- .rm_nonRLum(object)

      ## just return NULL
      if (length(l) == 0)
        return(NULL)

      ## apply method in the objects and return the same
      l <- lapply(object, function(x) {
        t <- try(melt_RLum(x), silent = TRUE)

        if (inherits(t, "try-error"))
          return(NULL)
        else
          t
      })

      ## remove NULL
      l <- l[!vapply(l, is.null, logical(1))]

      ## now bind the data.frame
      as.data.frame(data.table::rbindlist(l))
    })


## merge_RLum() --------------------------------------------------------------
#' @title Merge RLum-class objects
#'
#' @description
#' These functions merge [Luminescence::RLum-class] objects of the same class
#' without modifying the original objects. Empty list elements (`NULL`) are
#' automatically removed from the input list. The dispatch is based on the
#' class of the input objects:
#'
#' - [Luminescence::RLum.Analysis-class]: when at least one input is an
#' `RLum.Analysis` object, a single `RLum.Analysis` object is returned
#' containing the supplied [Luminescence::RLum.Data-class] objects and
#' `RLum.Analysis` records, in the order given.
#' - [Luminescence::RLum.Data.Curve-class] and
#' [Luminescence::RLum.Data.Spectrum-class]: merging is done element-wise
#' on the data values using one of several supported methods (see below).
#' - [Luminescence::RLum.Results-class]: data elements are appended or
#' combined depending on their type.
#'
#' @section Supported merge operations:
#'
#' The following values for the `merge.method` argument are supported when
#' merging [Luminescence::RLum.Data.Curve-class] and
#' [Luminescence::RLum.Data.Spectrum-class] objects:
#'
#' - `"mean"` (default): the mean over the count/cell values is calculated
#' using [rowMeans].
#'
#' - `"median"`: the median over the count/cell values is calculated using
#' [matrixStats::rowMedians].
#'
#' - `"sum"`: all count/cell values are summed up using [rowSums].
#'
#' - `"sd"`: the standard deviation over the count/cell values is calculated
#' using [matrixStats::rowSds].
#'
#' - `"var"`: the variance over the count/cell values is calculated using
#' [matrixStats::rowVars].
#'
#' - `"min"`: the min values from the count/cell values is calculated using
#' [matrixStats::rowMins].
#'
#' - `"max"`: the max values from the count/cell values is calculated using
#' [matrixStats::rowMaxs].
#'
#' - `"append"`: appends count/cell values of all objects to one combined data
#' object. The channel width is automatically re-calculated, but requires a
#' constant channel width of the original data.
#' **Note:** For [Luminescence::RLum.Data.Spectrum-class] objects, this method
#' is only available when all objects have the same number of columns.
#'
#' - `"-"`: the row sums of the last objects are subtracted from the first
#' object.
#'
#' - `"*"`: the row sums of the last objects are multiplied by the first
#' object.
#'
#' - `"/"`: values of the first object are divided by row sums of the last
#' objects.
#'
#' @param object [list] (**required**):
#' list of [Luminescence::RLum-class] objects to be merged. All elements must
#' be of the same type, unless at least one [Luminescence::RLum.Analysis-class]
#' object is present, in which case also [Luminescence::RLum.Data-class] can
#' be provided.
#'
#' @param ... further arguments passed to the specific class methods.
#'
#' @param merge.method [character] (*with default*):
#' method for combining of the objects, e.g. `'mean'` (default), `'median'`,
#' `'sum'`, see below for further information.
#' Only used for [Luminescence::RLum.Data.Curve-class] and
#' [Luminescence::RLum.Data.Spectrum-class] objects.
#'
#' @param method.info [numeric] (*optional*):
#' allows to specify how info elements of the input objects are combined,
#' e.g. `1` keeps only the info elements from the first object, `2` keeps only
#' those from the second object, etc. If set to `NULL` (default), all elements
#' are combined.
#' Only used for [Luminescence::RLum.Data.Curve-class] and
#' [Luminescence::RLum.Data.Spectrum-class] objects.
#'
#' @param max.temp.diff [numeric] (*with default*):
#' maximum difference in the time/temperature values between the spectra to
#' be merged: when differences exceed this threshold value, the merging
#' occurs but a warning is raised.
#' Only used for [Luminescence::RLum.Data.Spectrum-class] objects.
#'
#' @param flatten [logical] (*with default*):
#' whether list elements should be flattened before merging.
#' Only used for [Luminescence::RLum.Results-class] objects.
#'
#' @return
#' Returns an object of the same class as the input elements.
#'
#' @note
#' - For [Luminescence::RLum.Analysis-class] objects, the information for the
#' `protocol` slot is taken from the first [Luminescence::RLum.Analysis-class]
#' object in the input list.
#' - For [Luminescence::RLum.Data.Curve-class] and
#' [Luminescence::RLum.Data.Spectrum-class] objects, the information from the
#' `recordType` slot is taken from the first object in the input list. The
#' `curveType` slot is set to `"merged"`.
#' - For [Luminescence::RLum.Data.Image-class], no merging is supported.
#' - For [Luminescence::RLum.Results-class] objects, the `originator` is taken
#' from the first element and not reset to `"merge_RLum"`.
#'
#' @section S3 generic support:
#'
#' Merging of `RLum.Data.Curve` and `RLum.Data.Spectrum` objects can also be
#' performed via S3-generics: `+`, `-`, `/`, `*`.
#'
#' @section Function version: 0.3.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)\cr
#'
#' @seealso [Luminescence::RLum.Analysis-class],
#' [Luminescence::RLum.Data.Curve-class],
#' [Luminescence::RLum.Data.Spectrum-class],
#' [Luminescence::RLum.Results-class]
#'
#' @keywords utilities
#'
#' @examples
#'
#' ## load example data
#' data(ExampleData.XSYG, envir = environment())
#' data(ExampleData.DeValues, envir = environment())
#' data(ExampleData.RLum.Analysis, envir = environment())
#'
#' ## extract the first and third TL curves
#' TL.curves  <- get_RLum(OSL.SARMeasurement$Sequence.Object,
#'                        recordType = "TL (UVVIS)")
#'
#' ## ---- RLum.Analysis ----
#' merged <- merge_RLum(list(TL.curves[[1]], IRSAR.RF.Data, IRSAR.RF.Data))
#'
#' ## ---- RLum.Data.Curve ----
#' ## subtract the 1st curve from the 3rd
#' TL.merged <- merge_RLum(list(TL.curves[[3]], TL.curves[[1]]),
#'                         merge.method = "-")
#'
#' ## ---- RLum.Data.Spectrum ----
#' ## sum two copies of the same spectrum
#' spectrum.merged <- merge_RLum(list(TL.Spectrum, TL.Spectrum),
#'                               merge.method = "sum")
#'
#' ## ---- RLum.Results ----
#' res <- calc_CentralDose(ExampleData.DeValues$CA1)
#' res.merged <- merge_RLum(list(res, res))
#'
#' @name merge_RLum
#' @export
setGeneric("merge_RLum", function(object, ...) {
  .set_function_name("merge_RLum")
  on.exit(.unset_function_name(), add = TRUE)

  ## deprecated argument
  if ("objects" %in% ...names()) {
    object <- list(...)$objects
    .deprecated(old = "objects", new = "object", since = "1.3.1")
  }

  .validate_class(object, "list")
  standardGeneric("merge_RLum")
})


## names() ------------------------------------------------------------------
#' @title Names of RLum-class and Risoe-class objects
#'
#' @param x [Luminescence::RLum-class] or [Luminescence::Risoe.BINfileData-class] (**required**):
#' S4 object.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)\cr
#'
#' @keywords utilities
#' @name names
NULL


## add_metadata<-() ---------------------------------------------------------
#' @title Safe manipulation of object metadata
#'
#' @description
#' Generic functions for manipulation of metadata in [Luminescence::Risoe.BINfileData-class],
#' [Luminescence::RLum.Analysis-class] and [Luminescence::RLum.Data-class] objects.
#'
#' @param object [Luminescence::RLum.Analysis-class],
#' [Luminescence::Risoe.BINfileData-class] (**required**):
#' object of class `RLum.Analysis` or `Risoe.BINfileData` to manipulate.
#'
#' @param ... further arguments passed to the specific class method
#'
#' @param value (**required**):
#' value to be assigned to the selected metadata entry. A `NULL` value is
#' acceptable only for `replace_metadata`, in which case the elements named
#' in `info_element` will be removed.
#'
#' @author
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [Luminescence::RLum.Data-class], [Luminescence::RLum.Analysis-class],
#' [Luminescence::Risoe.BINfileData-class]
#'
#' @keywords utilities
#'
#' @examples
#'
#' ## load example data
#' data(ExampleData.BINfileData, envir = environment())
#'
#' ## show data
#' CWOSL.SAR.Data
#'
#' ## add a new field
#' add_metadata(CWOSL.SAR.Data,
#'              info_element = "INSTITUTE") <- "Heidelberg University"
#'
#' ## rename a field
#' rename_metadata(CWOSL.SAR.Data,
#'                 info_element = "INSTITUTE") <- "INSTITUTION"
#'
#' ## replace all LTYPE to RSL
#' ## but only for the first position
#' replace_metadata(
#'  object = CWOSL.SAR.Data,
#'  info_element = "LTYPE",
#'  subset = (POSITION == 1)) <- "RSL"
#'
#' ## replacing a field with NULL allows to remove that field
#' replace_metadata(CWOSL.SAR.Data,
#'                  info_element = "PREVIOUS") <- NULL
#'
#' ## show the modified data
#' CWOSL.SAR.Data
#'
#' @rdname metadata
#' @export
setGeneric("add_metadata<-", function(object, ..., value)
  standardGeneric("add_metadata<-")
)

#' @rdname metadata
#' @export
setGeneric("rename_metadata<-", function(object, ..., value)
  standardGeneric("rename_metadata<-")
)

#' @rdname metadata
#' @export
setGeneric("replace_metadata<-", function(object, ..., value)
  standardGeneric("replace_metadata<-")
)


## replicate_RLum() ---------------------------------------------------------
#' @title General replication function for RLum-class objects
#'
#' @description
#' The function replicates [Luminescence::RLum-class] objects and returns a list of such
#' objects.
#'
#' @param object [Luminescence::RLum-class] (**required**):
#' an [Luminescence::RLum-class] object
#'
#' @param times [integer] (*optional*):
#' number for times each element should be repeated.
#'
#' @return
#' A [list] with the object repeated.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @seealso [Luminescence::RLum-class]
#'
#' @keywords utilities
#'
#' @export
setGeneric("replicate_RLum", function(object, times = 1)
   standardGeneric("replicate_RLum")
)


## set_Risoe.BINfileData() --------------------------------------------------
#' @title General setter function for Risoe.BINfileData objects
#'
#' @description
#' The function provides a generalised access point for specific
#' [Luminescence::Risoe.BINfileData-class] objects. Depending on the input object, the
#' corresponding function will be selected.
#' Allowed arguments can be found in the documentations of the corresponding
#' [Luminescence::Risoe.BINfileData-class] class.
#'
#' @param METADATA x
#'
#' @param DATA x
#'
#' @param .RESERVED x
#'
#' @return
#' A [Luminescence::Risoe.BINfileData-class] object.
#'
#' @section Function version: 0.1
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @seealso [Luminescence::Risoe.BINfileData-class]
#'
#' @keywords utilities
#'
#' @export
setGeneric("set_Risoe.BINfileData", function(METADATA = data.frame(),
                                             DATA = list(), .RESERVED = list())
  standardGeneric("set_Risoe.BINfileData")
)


## set_RLum() ---------------------------------------------------------------
#' @title General setter function for RLum-class objects
#'
#' @description
#' The function provides a generalised access point for specific
#' [Luminescence::RLum-class] objects. Depending on the given class, the corresponding
#' method to create an object from this class will be selected.
#'
#' @param class [character] (**required**):
#' name of the S4 class to create, must correspond to one of the [Luminescence::RLum-class]
#' classes.
#'
#' @param originator [character] (*automatic*):
#' contains the name of the calling function (the function that produces this object);
#' can be set manually.
#'
#' @param .uid [character] (*automatic*):
#' unique ID for this object, by default set using the internal C++ function
#' `create_UID`.
#'
#' @param .pid [character] (*with default*):
#' option to provide a parent id for nesting at will.
#'
#' @param ... further arguments passed to the specific class method
#'
#' @return
#' An object of the specified [Luminescence::RLum-class] class.
#'
#' @section Function version: 0.3.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @seealso [Luminescence::RLum.Data.Curve-class], [Luminescence::RLum.Data.Image-class],
#' [Luminescence::RLum.Data.Spectrum-class], [Luminescence::RLum.Analysis-class],
#' [Luminescence::RLum.Results-class]
#'
#' @keywords utilities
#'
#' @examples
#'
#' ## produce empty objects from each class
#' set_RLum(class = "RLum.Data.Curve")
#' set_RLum(class = "RLum.Data.Spectrum")
#' set_RLum(class = "RLum.Data.Spectrum")
#' set_RLum(class = "RLum.Analysis")
#' set_RLum(class = "RLum.Results")
#'
#' ## produce a curve object with arbitrary curve values
#' object <- set_RLum(
#' class = "RLum.Data.Curve",
#' curveType = "arbitrary",
#' recordType = "OSL",
#' data = matrix(c(1:100,exp(-c(1:100))),ncol = 2))
#'
#' ## plot this curve object
#' plot_RLum(object)
#'
#' @export
setGeneric("set_RLum", function(class, originator, .uid = create_UID(),
                                .pid = NA_character_, ... ) {
  .set_function_name("set_RLum")
  on.exit(.unset_function_name(), add = TRUE)

  .validate_class(class, "character", length = 1)
  if (!nzchar(class)) {
    .throw_error("'class' cannot be an empty character")
  }
  class(class) <- as.character(class)

  if (missing(originator)) {
    caller <- sys.call(which = -1)[[1]]
    if (is.language(caller)) {
      ## use tail() to account for package-qualified calls
      originator <- tail(as.character(caller), 1)
    } else {
      ## try harder to find the originator by looking at the parent call: if
      ## it's do.call(), then take use the function it calls as originator
      parent.call <- if (length(sys.calls()) > 2) sys.call(which = -2) else NULL
      if (!is.null(parent.call) &&
          is.language(parent.call[[1]]) &&
          as.character(parent.call[[1]]) == "do.call") {
        originator <- as.character(parent.call[[2]])
      } else {
        originator <- NA_character_
      }
    }
  }

  standardGeneric("set_RLum")
})


## show() -------------------------------------------------------------------
#' @name show
#' @title Show the structure of RLum-class and Risoe.BINfileData-class objects
#'
#' @param object [Luminescence::RLum-class], [Luminescence::Risoe.BINfileData-class] (**required**):
#' object of class `RLum` or `Risoe.BINfileData`.
#'
NULL

## smooth_RLum() ------------------------------------------------------------
#' @title Smoothing of data for RLum-class objects
#'
#' @description
#' The function provides a generalised access point for specific
#' [Luminescence::RLum-class] objects. Depending on the input object, the corresponding
#' function will be selected. The smoothing is performed in the internal
#' function `.smoothing()`.
#'
#' @param object [Luminescence::RLum-class] (**required**):
#' S4 object of class `RLum`
#'
#' @param ... further arguments passed to the specific class method
#'
#' @return
#' An object of the same type as the input object provided.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @note
#' Currently only `RLum` objects of class `RLum.Data.Curve` and `RLum.Analysis`
#' (with curve data) are supported.
#'
#' @seealso [Luminescence::RLum.Data.Curve-class], [Luminescence::RLum.Analysis-class]
#'
#' @examples
#'
#' ## load example data
#' data(ExampleData.CW_OSL_Curve, envir = environment())
#'
#' ## create RLum.Data.Curve object from this example
#' curve <-
#'   set_RLum(
#'       class = "RLum.Data.Curve",
#'       recordType = "OSL",
#'       data = as.matrix(ExampleData.CW_OSL_Curve)
#'   )
#'
#' ## plot data without and with smoothing
#' plot_RLum(curve)
#' plot_RLum(smooth_RLum(curve))
#'
#' @keywords utilities
#'
#' @export
setGeneric("smooth_RLum", function(object, ...)
  standardGeneric("smooth_RLum")
)

#' @describeIn smooth_RLum
#' Returns a list of [Luminescence::RLum-class] objects that had been passed to
#' [Luminescence::smooth_RLum]
#'
#' @export
setMethod("smooth_RLum", signature = "list",
    function(object, ...) {
      ## apply method in the objects and return the same
      lapply(object, function(x) {
        if (inherits(x, "RLum")) {
          return(smooth_RLum(x,...))
        } else {
          return(x)
        }
      })
    })

## normalise_RLum() ------------------------------------------------------------
#' @title Normalisation of RLum-class objects
#'
#' @description
#' The function provides a generalised access point for specific
#' [Luminescence::RLum-class] objects. Depending on the input object, the corresponding
#' function will be selected. The normalisation is performed in the internal
#' function `.normalise_curve()`.
#'
#' @param object [Luminescence::RLum-class] (**required**):
#' S4 object of class `RLum`
#'
#' @param norm [logical] [character] (**required**):
#' if logical, whether curve normalisation should occur; alternatively, one
#' of `"max"` (used with `TRUE`), `"min"`, `"first"`, `"last"`, `"huot"`,
#' `"intensity"` or a positive number (e.g., 2.2).
#'
#' @param ... further arguments passed to the specific class method
#'
#' @details
#' The `norm` argument normalises all count values. The following options are
#' supported:
#'
#' `norm = TRUE` or `norm = "max"`: Curve values are normalised to the highest
#' count value in the curve
#'
#' `norm = "min"`: Curve values are normalised to the smallest count value
#' in the curve
#'
#' `norm = "first"`: Curve values are normalised to the first count value.
#'
#' `norm = "last"`: Curve values are normalised to the last count value
#' (this can be useful in particular for radiofluorescence curves)
#'
#' `norm = "huot"`: Curve values are normalised as suggested by Sébastien Huot
#'  via GitHub:
#' \deqn{
#' y = (observed - median(background)) / (\max(observed) - median(background))
#' }
#'
#' The background of the curve is defined as the last 20% of the count values
#' of a curve.
#'
#' `norm = "intensity"`: Curve values are normalised to the channel length.
#'
#' `norm = 2.2`: Curve values are normalised to a positive number (e.g., 2.2).
#'
#' @return
#' An object of the same type as the input object provided.
#'
#' @section Function version: 0.1.3
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @seealso [Luminescence::RLum.Data.Curve-class], [Luminescence::RLum.Analysis-class],
#' [Luminescence::RLum.Data.Spectrum-class],  [Luminescence::RLum.Data.Image-class]
#'
#' @examples
#'
#' ## load example data
#' data(ExampleData.CW_OSL_Curve, envir = environment())
#'
#' ## create RLum.Data.Curve object from this example
#' curve <-
#'   set_RLum(
#'       class = "RLum.Data.Curve",
#'       recordType = "OSL",
#'       data = as.matrix(ExampleData.CW_OSL_Curve)
#'   )
#'
#' ## plot data without and with smoothing
#' plot_RLum(curve)
#' plot_RLum(normalise_RLum(curve))
#'
#' @keywords utilities
#'
#' @export
setGeneric("normalise_RLum", function(object, norm = TRUE, ...) {
  .set_function_name("normalise_RLum")
  on.exit(.unset_function_name(), add = TRUE)

  ## validation
  .validate_class(norm, c("logical", "character", "numeric"), length = 1)
  valid.norms <- c("max", "min", "first", "last", "huot", "intensity")
  if(inherits(norm, "character"))
    .validate_args(norm, valid.norms)
  else if (inherits(norm, "logical"))
    .validate_logical_scalar(norm, extra = paste("one of", .collapse(valid.norms)))
  else if (inherits(norm, "numeric"))
    .validate_positive_scalar(norm, extra = paste("one of", .collapse(valid.norms)))

  ## set generic
  standardGeneric("normalise_RLum")
})

#' @describeIn normalise_RLum
#' Returns a list of [Luminescence::RLum.Data-class] objects that had been passed to
#' [Luminescence::normalise_RLum]
#'
#' @export
setMethod("normalise_RLum", signature = "list",
          function(object, norm, ...) {
            ## apply method in the objects and return the same
            lapply(object, function(x) {
              if (inherits(x, c("RLum.Analysis", "RLum.Data"))) {
                return(normalise_RLum(x, norm = norm, ...))
              } else {
                return(x)
              }
            })
          })

## sort_RLum() --------------------------------------------------------------
#' @title Sort data for RLum-class and Risoe.BINfileData-class objects
#'
#' @description
#' The function provides a generalised access point for specific [Luminescence::RLum-class]
#' and [Luminescence::Risoe.BINfileData-class] objects. Depending on the input object,
#' the corresponding function will be selected.
#'
#' @param object [Luminescence::RLum-class] or
#' [Luminescence::Risoe.BINfileData-class] (**required**):
#' S4 object of class `RLum.Analysis` or `Risoe.BINfileData`.
#'
#' @param ... further arguments passed to the specific class method
#'
#' @return
#' An object of the same type as the input object provided.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [Luminescence::RLum.Analysis-class], [Luminescence::Risoe.BINfileData-class]
#'
#' @examples
#'
#' ## load example data
#' data(ExampleData.XSYG, envir = environment())
#' obj <- OSL.SARMeasurement$Sequence.Object[1:9]
#'
#' sort_RLum(obj, slot = "recordType")
#' sort_RLum(obj, info_element = "curveDescripter")
#'
#' @keywords utilities
#'
#' @export
setGeneric("sort_RLum", function(object, ...)
  standardGeneric("sort_RLum")
)

#' @describeIn sort_RLum
#' Returns a list of sorted [Luminescence::RLum-class] objects.
#'
#' @export
setMethod("sort_RLum", signature = "list",
          function(object, ...) {
            ## apply method in the objects and return the same
            lapply(object, function(x) {
              if (inherits(x, "RLum.Analysis")) {
                return(sort_RLum(x,...))
              } else {
                return(x)
              }
            })
          })


## structure_RLum() ---------------------------------------------------------
#' @title General structure function for RLum-class objects
#'
#' @description
#' The function provides a generalised access point for specific
#' [Luminescence::RLum-class] objects. Depending on the input object, the corresponding
#' function will be selected.
#'
#' @param object [Luminescence::RLum-class] (**required**):
#' S4 object of class `RLum`
#'
#' @param ... further arguments passed to the specific class method
#'
#' @return
#' Returns a [data.frame] with structure of the object.
#'
#' @section Function version: 0.2.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @seealso [Luminescence::RLum.Data.Curve-class], [Luminescence::RLum.Data.Image-class],
#' [Luminescence::RLum.Data.Spectrum-class], [Luminescence::RLum.Analysis-class],
#' [Luminescence::RLum.Results-class]
#'
#' @keywords utilities
#'
#' @examples
#'
#' ## load example data
#' data(ExampleData.XSYG, envir = environment())
#'
#' ## show structure
#' structure_RLum(OSL.SARMeasurement$Sequence.Object)
#'
#' @export
setGeneric("structure_RLum", function(object, ...)
  standardGeneric("structure_RLum")
)

#' @describeIn structure_RLum
#' Returns a list of data frames containing the structure of each [Luminescence::RLum-class]
#' object in the input list.
#'
#' @export
setMethod("structure_RLum", signature = "list",
    function(object, ...) {
      ## apply method in the objects and return the same
      lapply(object, function(x) {
        if (inherits(x, "RLum")) {
          return(structure_RLum(x, ...))
        } else {
          return(x)
        }
      })
    })


## summary() ----------------------------------------------------------------
#' @title Summary of RLum-class objects
#'
#' @param object [Luminescence::RLum-class] (**required**):
#' S4 object of class `RLum`.
#'
#' @param ... further arguments passed to [base::summary].
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)\cr
#'
#' @keywords utilities
#' @name summary
NULL


## view() -------------------------------------------------------------------
#' @title Convenience data visualisation function
#'
#' @description
#' Invokes the [utils::View] function tailored to objects in the package.
#' If started from RStudio, it uses the RStudio viewer.
#'
#' @param object (**required**) object to view
#'
#' @param ... further arguments passed to the specific class method
#'
#' @seealso [utils::View()]
#'
#' @returns
#' `NULL` and opens the data viewer.
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @keywords utilities
#'
#' @export
setGeneric("view", function(object, ... )
  standardGeneric("view")
)

## ensure that we can use the internal RStudio view function
## https://stackoverflow.com/questions/48234850/how-to-use-r-studio-view-function-programatically-in-a-package
#' @noRd
.view <- function(x, title) {
  get("View", envir = as.environment("package:utils"))(x, title) # nocov
}
