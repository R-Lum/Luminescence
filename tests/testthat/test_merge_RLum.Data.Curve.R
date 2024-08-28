test_that("Merge tests", {
  testthat::skip_on_cran()

  ##load example data
  data(ExampleData.XSYG, envir = environment())
  TL.curves  <- get_RLum(OSL.SARMeasurement$Sequence.Object, recordType = "TL (UVVIS)")
  TL.curve.1 <- TL.curves[[1]]
  TL.curve.3 <- TL.curves[[3]]
  TL.curve.3_short <- TL.curves[[3]]
  TL.curve.3_short@data <-  TL.curve.3_short@data[1:(nrow(TL.curve.3_short@data) - 1),]
  TL.curve.3_types <- TL.curves[[3]]
  TL.curve.3_types@recordType <- "IRSL"
  TL.curve.3_zeros <- TL.curves[[3]]
  TL.curve.3_zeros@data[10:12, 2] <- 0

  ##check for error
  expect_error(merge_RLum.Data.Curve("", merge.method = "/"),
               "At least object 1 is not of class 'RLum.Data.Curve'")
  expect_error(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                                     merge.method = "error"),
               "Unsupported or unknown merge method")
  expect_error(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3_types)),
               "Only similar record types are supported")

  ## check warning for different curve lengths
  expect_warning(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3_short),
                                       merge.method = "mean"),
                 "The number of channels between the curves differs")

  ##check error for different resolution
  TL.curve.3_short@data <-  TL.curve.3_short@data[-2,]
  expect_error(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3_short),
                                     merge.method = "mean"),
               "The objects do not seem to have the same channel resolution")

  ##check various operations
  expect_s4_class(TL.curve.1 + TL.curve.3, "RLum.Data.Curve")
  expect_s4_class(TL.curve.1 - TL.curve.3, "RLum.Data.Curve")
  expect_s4_class(suppressWarnings(TL.curve.3 / TL.curve.1), "RLum.Data.Curve")
  expect_warning(TL.curve.3 / TL.curve.1)
  expect_s4_class(TL.curve.1 * TL.curve.3, "RLum.Data.Curve")

  merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                        merge.method = "sum", method.info = 1)
  merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                        merge.method = "median")
  merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                        merge.method = "sd")
  merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                        merge.method = "var")
  merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                        merge.method = "max")
  merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                        merge.method = "min")
  merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                        merge.method = "-")
  merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                        merge.method = "*")
  merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                        merge.method = "/")
  expect_warning(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3_zeros),
                                       merge.method = "/"),
                 "3 'inf' values have been replaced by 0 in the matrix")
  expect_warning(merge_RLum.Data.Curve(list(TL.curve.1, TL.curve.3),
                                     merge.method = "append"),
               "longer object length is not a multiple of shorter object length") # FIXME(mcol)
})
