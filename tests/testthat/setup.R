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
    if ("fits" %in% names(object@data)) { # calc_Huntley2006()
      if ("simulated" %in% names(object@data$fits)) {
        object@data$fits$simulated$m <- NULL
        object@data$fits$simulated$call <- NULL
        object@data$fits$simulated$weights <- NULL # for macos/windows CI
      }
      if ("measured" %in% names(object@data$fits)) {
        object@data$fits$measured$m <- NULL
        object@data$fits$measured$call <- NULL
      }
      if ("unfaded" %in% names(object@data$fits)) {
        object@data$fits$unfaded$m <- NULL
        object@data$fits$unfaded$call <- NULL
        object@data$fits$unfaded$convInfo$finIter <- NULL # for macos
      }
    }
    if ("data" %in% names(object@data))
      object@data$data$UID <- NULL
    if ("Fit" %in% names(object@data))
      object@data$Fit <- NULL
    if ("Formula" %in% names(object@data))
      object@data$Formula <- NULL
    if ("LnLxTnTx.table" %in% names(object@data))
      object@data$LnLxTnTx.table$UID <- NULL
    if ("rejection.criteria" %in% names(object@data))
      object@data$rejection.criteria$UID <- NULL
    if ("test_parameters" %in% names(object@data))
      object@data$test_parameters$UID <- NULL

    ## This should be removed once we do not run coverage
    ## anymore on R 4.3 (issue #312)
    if ("De" %in% names(object@data)) {
      object@data$De$HPDI95_L <- NULL
      object@data$De$HPDI95_U <- NULL
    }

    ## This should be removed once we do not run coverage
    ## anymore on R 4.3 (pull #420)
    if ("MC" %in% names(object@data) && "kde" %in% names(object@data$MC)) {
      if (!is.null(object@data$MC$kde$old.coords))
        object@data$MC$kde$old.coords <- NULL
    }

  }
  if ("info" %in% slotNames(object)) {
    if ("call" %in% names(object@info)) {
      object@info$call <- NULL
    }
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

## wrapper for Risoe.BINfileData objects
expect_snapshot_Risoe <- function(object, ...) {
  attr(object, ".S3Class") <- NULL
  expect_snapshot_value(object, style = "json2", ...)
}

## wrapper for plain R objects, such as lists, data.frames, etc
expect_snapshot_plain <- function(object, ...) {
  expect_snapshot_value(object, style = "json2", ...)
}
