test_that("Merge tests", {
  testthat::skip_on_cran()
  local_edition(3)

  ##load example data
  data(ExampleData.XSYG, envir = environment())
  TL.curves  <- get_RLum(OSL.SARMeasurement$Sequence.Object, recordType = "TL (UVVIS)")
  TL.curve.1 <- TL.curves[[1]]
  TL.curve.3 <- TL.curves[[3]]

  ##check for error
  expect_error(merge_RLum.Data.Curve("", merge.method = "/"))

  ##check various operations
  expect_s4_class(TL.curve.1 + TL.curve.3, "RLum.Data.Curve")
  expect_s4_class(TL.curve.1 - TL.curve.3, "RLum.Data.Curve")
  expect_s4_class(suppressWarnings(TL.curve.3 / TL.curve.1), "RLum.Data.Curve")
  expect_warning(TL.curve.3 / TL.curve.1)
  expect_s4_class(TL.curve.1 * TL.curve.3, "RLum.Data.Curve")

})
