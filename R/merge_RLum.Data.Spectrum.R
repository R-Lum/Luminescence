#' @rdname merge_RLum
setMethod("merge_RLum", signature(object = "list", .class = "RLum.Data.Spectrum"),
function(
  object,
  merge.method = c("mean", "median", "sum", "sd", "var", "min", "max",
                   "append", "-", "*", "/"),
  method.info = NULL,
  max.temp.diff = 0.1,
  .class
) {
  .set_function_name("merge_RLum.Data.Spectrum")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------
  merge.method <- .validate_merge_RLum.Data(object, merge.method, method.info)
  .validate_positive_scalar(max.temp.diff)

  ## Merge objects ----------------------------------------------------------

  ## perform additional checks
  check.rows <- vapply(object, function(x) nrow(x@data), numeric(1))
  check.cols <- vapply(object, function(x) ncol(x@data), numeric(1))
  if (any(check.cols < 2)) {
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

  ## collect the data slot from all objects: each spectrum is flattened into
  ## one column, yielding a (num.rows * num.cols) x num.objects matrix
  temp.matrix <- sapply(object, function(x) {
    ## row names must match exactly
    if (!identical(rownames(x@data), x.vals))
      .throw_error("'RLum.Data.Spectrum' objects with different channels ",
                   "cannot be merged")

    ## check the camera type
    if (!identical(x@info$cameraType, cameraType))
      .throw_error("'RLum.Data.Spectrum' objects from different camera types",
                   "cannot be merged")

    ## for time/temperature data we allow some small differences: we report
    ## a warning if they are too high, but continue anyway
    if (!is.null(colnames(x@data)) &&
        max(abs(as.numeric(colnames(x@data)) - y.vals)) > max.temp.diff) {
        .throw_warning("The time/temperatures recorded are too different, ",
                       "proceed with caution")
    }
    x@data
  })

  ## apply selected method for merging
  temp.matrix <- .merge_data_matrix(temp.matrix, merge.method)

  ## restore the two-dimensional layout of the spectrum: for 'append', the
  ## spectra are concatenated side by side, otherwise they are reduced to one
  num.reps <- if (merge.method == "append") length(object) else 1
  temp.matrix <- array(temp.matrix, c(check.rows[1], check.cols[1] * num.reps))

  ## restore row and column names from the first object
  rownames(temp.matrix) <- rownames(object[[1]]@data)
  colnames(temp.matrix) <- rep(colnames(object[[1]]@data), num.reps)

  ## add the info slot
  temp.info <- if (is.null(method.info)) {
                 unlist(lapply(object, function(x) x@info), recursive = FALSE)
               } else {
                 object[[method.info]]@info
               }

  ## Build new RLum.Data.Spectrum object ------------------------------------
  set_RLum(
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
})
