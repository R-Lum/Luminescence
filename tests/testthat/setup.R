## --------------------------------------------------------------------------
## helper functions for snapshotting
##

## the ... can be used to set the tolerance
expect_snapshot_RLum <- function(object, ...) {
  object@.uid <- NA_character_
  object@.pid <- NA_character_
  object@info$call <- NULL
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
