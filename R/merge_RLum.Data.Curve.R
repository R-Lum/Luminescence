#' @rdname merge_RLum
setMethod("merge_RLum", signature(object = "list", .class = "RLum.Data.Curve"),
function(
  object,
  merge.method = c("mean", "median", "sum", "sd", "var", "min", "max",
                   "append", "-", "*", "/"),
  method.info = NULL,
  .class
) {
  .set_function_name("merge_RLum.Data.Curve")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------
  merge.method <- .validate_merge_RLum.Data(object, merge.method, method.info)

  ## Merge objects ----------------------------------------------------------
  ##merge data objects
  ##problem ... how to handle data with different resolution or length?

  ##(1) build new data matrix
  ## first find the shortest object
  check.rows <- vapply(object, function(x) nrow(x@data), numeric(1))
  if (min(check.rows) < 2) {
    .throw_error("'object' contains no data")
  }
  num.rows <- min(check.rows)

  ## channel resolution of the first object: we need to round as there may
  ## otherwise be numerical artefacts that would make the step not unique
  step <- round(diff(object[[1]]@data[, 1]), 1)[1]

  ## extract the curve values from each object
  temp.matrix <- sapply(object, function(x) {
    ## check the resolution (roughly)
    if (round(diff(x@data[, 1]), 1)[1] != step)
      .throw_warning("The curves do not seem to have the same channel resolution")
    ## limit all objects to the shortest one
    x@data[1:num.rows, 2]
  })

  ## throw the warning only now to avoid printing it in case of error
  if (length(unique(check.rows)) != 1) {
    .throw_warning("The number of channels differs between the curves, the ",
                   "merged curve will have the length of the shortest object")
  }

  ##(2) apply selected method for merging
  temp.matrix <- .merge_data_matrix(temp.matrix, merge.method)

  ## add back the first column to RLum.Data.Curve objects
  #If we append the data of the second to the first curve we have to recalculate
  #the x-values (probably time/channel). The difference should always be the
  #same, so we just expand the sequence if this is true. If this is not true,
  #we revert to the default behaviour (i.e., append the x values)
  if (merge.method == "append") {
    newx <- seq(from = min(object[[1]]@data[, 1]), by = step,
                length.out = sum(check.rows))
    temp.matrix <- cbind(newx, temp.matrix)
  } else {
    temp.matrix <- cbind(object[[1]]@data[1:num.rows, 1], temp.matrix)
  }

  ## remove spurious column names added by cbind()
  temp.matrix <- unname(temp.matrix)

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##merge info objects as simple as possible ... just keep them all ... other possibility
  ##would be to choose on the input objects

  ##unlist is needed here, as otherwise it would cause unexpected behaviour further using
  ##the RLum.object
  if (is.null(method.info)) {
    temp.info <- unlist(lapply(object, function(x) x@info), recursive = FALSE)
  }else{
    temp.info <- object[[method.info]]@info
  }

  ## Build new RLum.Data.Curve object ---------------------------------------
  set_RLum(
    class = "RLum.Data.Curve",
    originator = "merge_RLum.Data.Curve",
    recordType = object[[1]]@recordType,
    curveType =  "merged",
    data = temp.matrix,
    info = temp.info,
    .pid = unlist(lapply(object, function(x) x@.uid))
  )
})

## validate the inputs shared by merge_RLum.Data.Curve and merge_RLum.Data.Spectrum
.validate_merge_RLum.Data <- function(object, merge.method, method.info) {

  ## check for similar record types
  record.types <- unique(vapply(object, function(x) x@recordType, character(1)))
  if (length(record.types) > 1) {
    .throw_error("Objects cannot be merged, different record types found: ",
                 .collapse(record.types))
  }

  ## validate merge.method and method.info
  merge.method <- .validate_args(merge.method,
                                 c("mean", "median", "sum", "sd", "var",
                                   "min", "max", "append", "-", "*", "/"))
  .validate_positive_scalar(method.info, int = TRUE, null.ok = TRUE)
  if (!is.null(method.info) && method.info > length(object))
    .throw_error("'method.info' cannot exceed the number of objects being merged (",
                 length(object), ")")

  merge.method
}

## apply the selected merge method to the data matrix
.merge_data_matrix <- function(data, merge.method) {
  switch(merge.method,
         sum = rowSums(data),
         mean = rowMeans(data),
         median = matrixStats::rowMedians(data),
         sd = matrixStats::rowSds(data),
         var = matrixStats::rowVars(data),
         min = matrixStats::rowMins(data),
         max = matrixStats::rowMaxs(data),
         append = as.vector(data),
         "-" = data[, 1] - rowSums(data[, -1, drop = FALSE]),
         "*" = data[, 1] * rowSums(data[, -1, drop = FALSE]),
         "/" = {
           temp <- data[, 1] / rowSums(data[, -1, drop = FALSE])

           ## replace infinities with 0 and throw warning
           idx.inf <- which(is.infinite(temp))
           if (length(idx.inf) > 0) {
             temp[idx.inf]  <- 0
             .throw_warning(length(idx.inf),
                            " Inf values replaced by 0 in the matrix")
           }
           temp
         })
}
