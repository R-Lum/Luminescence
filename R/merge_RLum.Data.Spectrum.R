#' @title Merge function for RLum.Data.Spectrum S4 class objects
#'
#' @description
#' This function allows to merge [RLum.Data.Spectrum-class] objects in
#' different ways without modifying the original objects.
#'
#' @details
#'
#' **Supported merge operations are:**
#'
#' `"mean"` (default)
#'
#' The mean over the cell values is calculated using the function
#' [rowMeans].
#'
#' `"median"`
#'
#' The median over the cell values is calculated using the function
#' [matrixStats::rowMedians].
#'
#' `"sum"`
#'
#' All cell values will be summed up using the function [rowSums].
#'
#' `"sd"`
#'
#' The standard deviation over the cell values is calculated using the function
#' [matrixStats::rowSds].
#'
#' `"var"`
#'
#' The variance over the cell values is calculated using the function
#' [matrixStats::rowVars].
#'
#' `"min"`
#'
#' The min values from the cell values is chosen using the function
#' [matrixStats::rowMins][matrixStats::rowRanges].
#'
#' `"max"`
#'
#' The max values from the cell values is chosen using the function
#' [matrixStats::rowMins][matrixStats::rowRanges].
#'
#' `"append"` (only for [RLum.Data.Curve-class])
#'
#' Appends cell values of all curves to one combined data curve. The channel width
#' is automatically re-calculated, but requires a constant channel width of the
#' original data.
#'
#' `"-"`
#'
#' The cell sums of the last objects are subtracted from the first object.
#'
#' `"*"`
#'
#' The cell sums of the last objects are multiplied with the first object.
#'
#' `"/"`
#'
#' Values of the first object are divided by cell sums of the last objects.
#'
#' @param object [list] of [RLum.Data.Spectrum-class] (**required**):
#' list of objects to be merged.
#'
#' @param merge.method [character] (**required**):
#' method for combining of the objects, e.g. `'mean'` (default), `'median'`,
#' `'sum'`, see details for
#' further information and allowed methods.  Note: Elements in slot info will
#' be taken from the first object in the list.
#'
#' @param method.info [numeric] (*optional*):
#' allows to specify how info elements of the input objects are combined,
#' e.g. `1` means that just the elements from the first object are kept,
#' `2` keeps only the info elements from the 2 object etc.
#' If nothing is provided all elements are combined.
#'
#' @param max.temp.diff [numeric] (*with default*):
#' maximum difference in the time/temperature values between the spectra to
#' be merged: when differences exceed this threshold value, the merging
#' occurs but a warning is raised.
#'
#' @return Returns an [RLum.Data.Spectrum-class] object.
#'
#' @note
#' The information from the slot `recordType` is taken from the first
#' object in the input list. The slot
#' 'curveType' is filled with the name `merged`.
#'
#' @section S3-generic support:
#'
#' This function is fully operational via S3-generics:
#' ``+``, ``-``, ``/``, ``*``, `merge`
#'
#' @section Function version: 0.1.1
#'
#' @author
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [merge_RLum], [RLum.Data.Spectrum-class]
#'
#' @keywords utilities internal
#'
#' @examples
#'
#' ## load example data
#' data(ExampleData.XSYG, envir = environment())
#'
#' ## plot single curve
#' plot_RLum(TL.Spectrum)
#'
#' ## sum two copies of the same curve
#' merged <- merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum),
#'                                    merge.method = "sum")
#' plot_RLum(merged)
#'
#' @md
#' @export
merge_RLum.Data.Spectrum <- function(
  object,
  merge.method = "mean",
  method.info,
  max.temp.diff = 0.1
) {
  .set_function_name("merge_RLum.Data.Spectrum")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(object, "list")

  ## check if object is of a supported RLum.Data class
  num.objects <- length(object)
  temp.recordType.test <- sapply(object, function(x) {
    .validate_class(x, "RLum.Data.Spectrum",
                    name = "All elements of 'object'")
    return(x@recordType)
  })

  ## check for similar record types
  record.types <- unique(temp.recordType.test)
  if (length(record.types) > 1) {
    .throw_error("Only similar record types are supported; you are trying to merge: ",
                 .collapse(record.types))
  }

  merge.method <- .validate_args(merge.method,
                                 c("mean", "median", "sum", "sd", "var",
                                   "min", "max", "append", "-", "*", "/"))
  .validate_positive_scalar(max.temp.diff)

  ## Merge objects ----------------------------------------------------------

  ## perform additional checks
  check.rows <- vapply(object, function(x) nrow(x@data), numeric(1))
  check.cols <- vapply(object, function(x) ncol(x@data), numeric(1))
  if (length(check.rows) == 0 || length(check.cols) == 0) {
    .throw_error("'object' contains no data")
  }
  if (length(unique(check.rows)) > 1 || length(unique(check.cols)) > 1) {
    .throw_error("'RLum.Data.Spectrum' objects of different size ",
                 "cannot be merged")
  }

  ## collect the spectrum data from all objects
  x.vals <- rownames(object[[1]]@data)
  y.vals <- as.numeric(colnames(object[[1]]@data))
  cameraType <- object[[1]]@info$cameraType
  temp.matrix <- sapply(1:num.objects, function(x) {
    ## row names must match exactly
    if (!identical(rownames(object[[x]]@data), x.vals))
      .throw_error("'RLum.Data.Spectrum' objects with different channels ",
                   "cannot be merged")

    ## check the camera type
    if (!identical(object[[x]]@info$cameraType, cameraType))
      .throw_error("'RLum.Data.Spectrum' objects from different camera types",
                   "cannot be merged")

    ## for time/temperature data we allow some small differences: we report
    ## a warning if they are too high, but continue anyway
    if (max(abs(as.numeric(colnames(object[[x]]@data)) - y.vals)) > max.temp.diff) {
      .throw_warning("The time/temperatures recorded are too different, ",
                     "proceed with caution")
    }

    object[[x]]@data
  })

  ## reshape all spectrum data into a 3D array
  num.rows <- check.rows[1]
  num.cols <- check.cols[1]
  temp.matrix <- array(temp.matrix, c(num.rows, num.cols, num.objects))

  if (merge.method == "sum") {
    temp.matrix <- apply(temp.matrix, 2, rowSums)

  } else if (merge.method == "mean") {
    temp.matrix <- apply(temp.matrix, 2, rowMeans)

  } else if (merge.method == "median") {
    temp.matrix <- apply(temp.matrix, 2, matrixStats::rowMedians)

  } else if (merge.method == "sd") {
    temp.matrix <- apply(temp.matrix, 2, matrixStats::rowSds)

  } else if (merge.method == "var") {
    temp.matrix <- apply(temp.matrix, 2, matrixStats::rowVars)

  } else if (merge.method == "max") {
    temp.matrix <- apply(temp.matrix, 2, matrixStats::rowMaxs)

  } else if (merge.method == "min") {
    temp.matrix <- apply(temp.matrix, 2, matrixStats::rowMins)

  } else if (merge.method == "append") {
    temp.matrix <- array(temp.matrix, c(num.rows, num.cols * num.objects))

  } else if (merge.method == "-") {
    if (num.objects > 2) {
      temp.matrix <- temp.matrix[, , 1] - rowSums(temp.matrix[, , -1])
    } else {
      temp.matrix <- temp.matrix[, , 1] - temp.matrix[, , 2]
    }
  } else if (merge.method == "*") {
    if (num.objects > 2) {
      temp.matrix <- temp.matrix[, , 1] * rowSums(temp.matrix[, , -1])
    } else {
      temp.matrix <- temp.matrix[, , 1] * temp.matrix[, , 2]
    }
  } else if (merge.method == "/") {
    if (num.objects > 2) {
      temp.matrix <- temp.matrix[, , 1] / rowSums(temp.matrix[, , -1])
    } else {
      temp.matrix <- temp.matrix[, , 1] / temp.matrix[, , 2]
    }

    ## replace infinities with 0 and throw warning
    idx.inf <- which(is.infinite(temp.matrix))
    if (length(idx.inf) > 0) {
      temp.matrix[idx.inf]  <- 0
      .throw_warning(length(idx.inf),
                     " 'inf' values have been replaced by 0 in the matrix")
    }
  }

  ## restore row and column names from the first object
  rownames(temp.matrix) <- rownames(object[[1]]@data)
  colnames(temp.matrix) <- rep(colnames(object[[1]]@data),
                               if (merge.method == "append") num.objects else 1)

  ## add the info slot
  if (missing(method.info)) {
    temp.info <- unlist(lapply(1:num.objects, function(x) {
      object[[x]]@info
    }), recursive = FALSE)

  } else {
    temp.info <- object[[method.info]]@info
  }

  ## Build new RLum.Data.Spectrum object ------------------------------------
  new.Data.Spectrum <- set_RLum(
    class = as.character(class(object[[1]])),
    originator = "merge_RLum.Data.Spectrum",
    recordType = object[[1]]@recordType,
    curveType =  "merged",
    data = temp.matrix,
    info = temp.info,
    .pid = unlist(lapply(object, function(x) {
      x@.uid
    }))
  )

  return(new.Data.Spectrum)
}
