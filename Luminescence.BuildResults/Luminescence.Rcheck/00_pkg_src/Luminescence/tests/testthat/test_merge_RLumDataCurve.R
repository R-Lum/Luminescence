context("merge_RLum.Data.Curve")

test_that("Merge tests", {
  testthat::skip_on_cran()

  ##load example data
  data(ExampleData.XSYG, envir = environment())
  TL.curves  <- get_RLum(OSL.SARMeasurement$Sequence.Object, recordType = "TL (UVVIS)")
  TL.curve.1 <- TL.curves[[1]]
  TL.curve.3 <- TL.curves[[3]]

  ##check for error
  expect_error(merge_RLum.Data.Curve("", merge.method = "/"))

  ##check various operations
  expect_is(TL.curve.1 + TL.curve.3, "RLum.Data.Curve")
  expect_is(TL.curve.1 - TL.curve.3, "RLum.Data.Curve")
  expect_is(TL.curve.3 / TL.curve.1, "RLum.Data.Curve")
  expect_warning(TL.curve.3 / TL.curve.1)
  expect_is(TL.curve.1 * TL.curve.3, "RLum.Data.Curve")

})
