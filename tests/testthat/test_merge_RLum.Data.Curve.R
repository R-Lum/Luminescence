## load example data
data(ExampleData.XSYG, envir = environment())

TL.curves <- get_RLum(OSL.SARMeasurement$Sequence.Object,
                      recordType = "TL (UVVIS)")
TL.curve.1 <- TL.curves[[1]]
TL.curve.3 <- TL.curves[[3]]
TL.curve.3_short <- TL.curves[[3]]
TL.curve.3_short@data <- TL.curve.3_short@data[1:(nrow(TL.curve.3@data) - 1), ]
TL.curve.3_resol <- TL.curve.3_short
TL.curve.3_resol@data <- TL.curve.3_resol@data[-2, ]
TL.curve.3_types <- TL.curves[[3]]
TL.curve.3_types@recordType <- "IRSL"
TL.curve.3_zeros <- TL.curves[[3]]
TL.curve.3_zeros@data[10:12, 2] <- 0

test_that("input validation", {
  testthat::skip_on_cran()

  ##check for error
  expect_error(merge_RLum.Data.Curve("", merge.method = "/"),
               "All elements of 'object' should be of class 'RLum.Data.Curve'")
  expect_error(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                                     merge.method = "error"),
               "'merge.method' should be one of 'mean', 'median', 'sum', 'sd'")
  expect_error(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3_types)),
               "Only similar record types are supported")

  ## check warning for different curve lengths
  expect_warning(res <- merge_RLum.Data.Curve(list(TL.curve.1,
                                                   TL.curve.3_short)),
                 "The number of channels between the curves differs")
  expect_equal(nrow(res@data), nrow(TL.curve.3_short@data))

  ##check error for different resolution
  expect_error(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3_resol)),
               "The objects do not seem to have the same channel resolution")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_s4_class(TL.curve.1 + TL.curve.3, "RLum.Data.Curve")
  expect_s4_class(TL.curve.1 - TL.curve.3, "RLum.Data.Curve")
  expect_s4_class(suppressWarnings(TL.curve.3 / TL.curve.1), "RLum.Data.Curve")
  expect_warning(TL.curve.3 / TL.curve.1)
  expect_s4_class(TL.curve.1 * TL.curve.3, "RLum.Data.Curve")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  expect_snapshot_RLum(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                                             method.info = 1))
  expect_snapshot_RLum(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                                             merge.method = "sum"))
  expect_snapshot_RLum(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                                             merge.method = "median"))

  expect_snapshot_RLum(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                                             merge.method = "sd"))

  expect_snapshot_RLum(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                                             merge.method = "var"))

  expect_snapshot_RLum(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                                             merge.method = "max"))

  expect_snapshot_RLum(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                                             merge.method = "min"))

  expect_snapshot_RLum(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                                             merge.method = "-"))

  expect_snapshot_RLum(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                                             merge.method = "*"))

  expect_snapshot_RLum(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                                             merge.method = "/"))

  expect_warning(
      expect_snapshot_RLum(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3_zeros),
                                                 merge.method = "/")),
      "3 'inf' values have been replaced by 0 in the matrix")
  expect_snapshot_RLum(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                                             merge.method = "append"))
})
