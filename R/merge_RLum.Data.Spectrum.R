#' @rdname merge_RLum.Data.Curve
#' @export
merge_RLum.Data.Spectrum <- function(
  object,
  merge.method = c("mean", "median", "sum", "sd", "var", "min", "max",
                   "append", "-", "*", "/"),
  method.info = NULL,
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
    .throw_error("Objects cannot be merged, different record types found: ",
                 .collapse(record.types))
  }

  merge.method <- .validate_args(merge.method,
                                 c("mean", "median", "sum", "sd", "var",
                                   "min", "max", "append", "-", "*", "/"))
  .validate_positive_scalar(method.info, int = TRUE, null.ok = TRUE)
  if (!is.null(method.info) && method.info > num.objects)
    .throw_error("'method.info' cannot exceed the number of objects being merged (",
                 num.objects, ")")
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
    if (!is.null(colnames(x@data))) {
      if (max(abs(as.numeric(colnames(x@data)) - y.vals)) > max.temp.diff) {
        .throw_warning("The time/temperatures recorded are too different, ",
                       "proceed with caution")
      }
    }

    x@data
  })

  ## reshape all spectrum data into a 3D array
  num.rows <- check.rows[1]
  num.cols <- check.cols[1]
  temp.matrix <- array(temp.matrix, c(num.rows, num.cols, num.objects))

  temp.matrix <- switch(merge.method,
                        sum = apply(temp.matrix, 2, rowSums),
                        mean = apply(temp.matrix, 2, rowMeans),
                        median = apply(temp.matrix, 2, matrixStats::rowMedians),
                        sd = apply(temp.matrix, 2, matrixStats::rowSds),
                        var = apply(temp.matrix, 2, matrixStats::rowVars),
                        max = apply(temp.matrix, 2, matrixStats::rowMaxs),
                        min = apply(temp.matrix, 2, matrixStats::rowMins),
                        append = array(temp.matrix, c(num.rows, num.cols * num.objects)),
                        "-" = {
                          if (num.objects > 2) {
                            temp.matrix[, , 1] - rowSums(temp.matrix[, , -1])
                          } else {
                            temp.matrix[, , 1] - temp.matrix[, , 2]
                          }
                        },
                        "*" = {
                          if (num.objects > 2) {
                            temp.matrix[, , 1] * rowSums(temp.matrix[, , -1])
                          } else {
                            temp.matrix[, , 1] * temp.matrix[, , 2]
                          }
                        },
                        "/" = {
                          temp <- if (num.objects > 2) {
                                    temp.matrix[, , 1] / rowSums(temp.matrix[, , -1])
                                  } else {
                                    temp.matrix[, , 1] / temp.matrix[, , 2]
                                  }

                          ## replace infinities with 0 and throw warning
                          idx.inf <- which(is.infinite(temp))
                          if (length(idx.inf) > 0) {
                            temp[idx.inf]  <- 0
                            .throw_warning(length(idx.inf),
                                           " 'inf' values replaced by 0 in the matrix")
                          }
                          temp
                        })

  ## restore row and column names from the first object
  rownames(temp.matrix) <- rownames(object[[1]]@data)
  colnames(temp.matrix) <- rep(colnames(object[[1]]@data),
                               if (merge.method == "append") num.objects else 1)

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
}
