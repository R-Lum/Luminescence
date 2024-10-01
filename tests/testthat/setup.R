## --------------------------------------------------------------------------
## helper functions for snapshotting
##

## the ... can be used to set the tolerance
expect_snapshot_RLum <- function(object, ...) {
  object@.uid <- NA_character_
  object@.pid <- NA_character_
  object@info$call <- NULL
  if ("data" %in% slotNames(object)) {
    if ("fit" %in% names(object@data))
      object@data$fit <- NULL
    if ("data" %in% names(object@data))
      object@data$data$UID <- NULL
    if ("test_parameters" %in% names(object@data))
      object@data$test_parameters$UID <- NULL
  }
  if ("records" %in% slotNames(object)) {
    for (idx in seq_along(object@records)) {
      object@records[[idx]]@info$args <- NULL
      object@records[[idx]]@info$call <- NULL
      object@records[[idx]]@.uid <- NA_character_
      object@records[[idx]]@.pid <- NA_character_
    }
  }
  expect_snapshot_value(object, style = "json2", ...)
}

## wrapper for plain R objects, such as lists, data.frames, etc
expect_snapshot_plain <- function(object, ...) {
  expect_snapshot_value(object, style = "json2", ...)
}
