#' @rdname merge_RLum
merge_RLum.Data.Curve<- function(
  object,
  merge.method = c("mean", "median", "sum", "sd", "var", "max", "min",
                   "append", "-", "*", "/"),
  method.info = NULL,
  ...
) {
  .set_function_name("merge_RLum.Data.Curve")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------
  .validate_class(object, "list")
  .validate_positive_scalar(method.info, int = TRUE, null.ok = TRUE)
  if (!is.null(method.info) && method.info > length(object))
    .throw_error("'method.info' cannot exceed the number of objects being merged (",
                 length(object), ")")

  ##(1) check if object is of class RLum.Data.Curve
  temp.recordType.test <- sapply(object, function(x) {
    .validate_class(x, "RLum.Data.Curve",
                    name = "All elements of 'object'")
    return(x@recordType)
  })

  ##(2) Check for similar record types
  record.types <- unique(temp.recordType.test)
  if (length(record.types) > 1) {
    .throw_error("Objects cannot be merged, different record types found: ",
                 .collapse(record.types))
  }

  merge.method <- .validate_args(merge.method,
                                 c("mean", "median", "sum", "sd", "var", "max",
                                   "min", "append", "-", "*", "/"))

  ## Merge objects ----------------------------------------------------------
  ##merge data objects
  ##problem ... how to handle data with different resolution or length?

  ##(1) build new data matrix
  ## first find the shortest object
  check.rows <- vapply(object, function(x) nrow(x@data), numeric(1))
  if (length(check.rows) < 1 || min(check.rows) < 2) {
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
  temp.matrix <- switch(merge.method,
                        sum = rowSums(temp.matrix),
                        mean = rowMeans(temp.matrix),
                        median = matrixStats::rowMedians(temp.matrix),
                        sd = matrixStats::rowSds(temp.matrix),
                        var = matrixStats::rowVars(temp.matrix),
                        max = matrixStats::rowMaxs(temp.matrix),
                        min = matrixStats::rowMins(temp.matrix),
                        append = sapply(temp.matrix, c),
                        "-" = {
                          temp.matrix[, 1] - rowSums(temp.matrix[, -1, drop = FALSE])
                        },
                        "*" = {
                          temp.matrix[, 1] * rowSums(temp.matrix[, -1, drop = FALSE])
                        },
                        "/" = {
                          temp <- temp.matrix[, 1] / rowSums(temp.matrix[, -1, drop = FALSE])

                          ## replace infinities with 0 and throw warning
                          id.inf <- which(is.infinite(temp))
                          if (length(id.inf) > 0) {
                            temp[id.inf]  <- 0
                            .throw_warning(length(id.inf),
                                           " 'Inf' values replaced by 0 in the matrix")
                          }
                          temp
                        })

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
}
