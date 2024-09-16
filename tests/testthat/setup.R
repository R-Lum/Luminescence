## --------------------------------------------------------------------------
## helper functions for snapshotting
##

## the ... can be used to set the tolerance
expect_snapshot_RLum <- function(object, ...) {
  object@.uid <- NA_character_
  object@.pid <- NA_character_
  expect_snapshot_value(object, style = "json2", ...)
}
