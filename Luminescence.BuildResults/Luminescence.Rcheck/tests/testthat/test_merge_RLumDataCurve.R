test_that("Merge tests", {
  testthat::skip_on_cran()
  local_edition(3)

  ##load example data
  data(ExampleData.XSYG, envir = environment())
  TL.curves  <- get_RLum(OSL.SARMeasurement$Sequence.Object, recordType = "TL (UVVIS)")
  TL.curve.1 <- TL.curves[[1]]
  TL.curve.3 <- TL.curves[[3]]
  TL.curve.3_short <- TL.curves[[3]]
  TL.curve.3_short@data <-  TL.curve.3_short@data[1:(nrow(TL.curve.3_short@data) - 1),]

  ##check for error
  expect_error(merge_RLum.Data.Curve("", merge.method = "/"))

  ## check warning for different curve lengths
  expect_warning(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3_short), merge.method = "mean"))

  ##check error for different resolution
  TL.curve.3_short@data <-  TL.curve.3_short@data[-2,]
  expect_error(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3_short), merge.method = "mean"),
          "\\[merge\\_RLum.Data.Curve\\(\\)\\] The objects do not seem to have the same channel resolution!")

  ##check various operations
  expect_s4_class(TL.curve.1 + TL.curve.3, "RLum.Data.Curve")
  expect_s4_class(TL.curve.1 - TL.curve.3, "RLum.Data.Curve")
  expect_s4_class(suppressWarnings(TL.curve.3 / TL.curve.1), "RLum.Data.Curve")
  expect_warning(TL.curve.3 / TL.curve.1)
  expect_s4_class(TL.curve.1 * TL.curve.3, "RLum.Data.Curve")

})
