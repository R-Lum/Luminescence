#' @title Merge function for RLum.Results S4-class objects
#'
#' @description Function merges objects of class [Luminescence::RLum.Results-class]. The slots in the objects
#' are combined depending on the object type, e.g., for [data.frame] and [matrix]
#' rows are appended.
#'
#' @details Elements are appended where possible and attributes are preserved if
#' not of similar name as the default attributes of, e.g., a [data.frame]
#'
#' @note The `originator` is taken from the first element and not reset to [Luminescence::merge_RLum]
#'
#' @param objects [list] (**required**):
#' a list of [Luminescence::RLum.Results-class] objects
#'
#' @param flatten [logical] (*with default*):
#' whether list elements should be flattened before merging.
#'
#' @section Function version: 0.3
#'
#' @keywords internal
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @export
merge_RLum.Results <- function(
  objects,
  flatten = TRUE
) {
  .set_function_name("merge_RLum.Results")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------
  .validate_class(objects, "list")
  if (length(objects) == 0) {
    .throw_message("'objects' contains no data, NULL returned")
    return(NULL)
  }
  .validate_logical_scalar(flatten)

  ## check if objects in the list are of type RLum.Results
  temp.originator <- vapply(objects, function(x) {
    .validate_class(x, "RLum.Results", name = "All elements of 'object'")
    x@originator
  }, character(1))

  ## check if there are multiple originators
  if (length(unique(temp.originator)) > 1) {
    .throw_error("Objects cannot be merged, different originators found: ",
                 .collapse(temp.originator))
  }

  ## ------------------------------------------------------------------------
  ## merge each data element of the first object with the corresponding
  ## elements of all other objects
  data <- objects[[1]]@data
  for (i in seq_along(data)) {

    ## extract the elements from all all objects
    elements <- lapply(objects, function(x) x@data[[i]])

    ## data.frame, matrix or numeric vector
    if (inherits(elements[[1]], c("data.frame", "matrix", "numeric"))) {

      ## check whether the objects can be combined by rbind
      if (length(unique(vapply(elements, NCOL, integer(1)))) > 1)
        .throw_error("Objects cannot be merged, different number of columns")

      ##combine them using rbind or data.table::rbindList (depends on the data type)
      if (inherits(data[[i]], "numeric")) {
        data[[i]] <- unlist(elements)
      } else if (inherits(data[[i]], "matrix")) {
        data[[i]] <- do.call("rbind", elements)
      } else {
        data[[i]] <- as.data.frame(data.table::rbindlist(elements))
      }

      ## list of attributes
      attr_list <- unlist(lapply(elements, attributes),
                          recursive = FALSE)

      ## preserve attributes: keep those that are not recreated by the merging
      ## operation and combine attributes with the same name
      ## remove attributes that stem from the object itself
      attr_names <- setdiff(names(attr_list), names(attributes(data[[i]])))

      if (length(attr_names) > 0) {
        for (n in unique(attr_names)) {
          values <- unlist(lapply(elements, attr, which = n), use.names = FALSE)
          attr(data[[i]], n) <- values
        }
      }

    } else {
      ## all other elements: collect them into a list
      data[[i]] <- elements

      ## flatten nested lists if requested
      if (inherits(data[[i]][[1]], "list") && flatten) {
        data[[i]] <- unlist(data[[i]], recursive = FALSE)
      }
    }
  }

  ## return by setting a new RLum.Results (for the .uid)
  ## the originator is not reset
  set_RLum(
      class = "RLum.Results",
      originator = objects[[1]]@originator,
      data = data,
      info = unlist(lapply(objects, function(x) x@info), recursive = FALSE),
      .pid = unlist(lapply(objects, function(x) x@.uid))
  )
}
