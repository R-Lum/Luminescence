## load example data
data(ExampleData.XSYG, envir = environment())
TL.Spectrum_types <- TL.Spectrum
TL.Spectrum_types@recordType <- "OSL (Spectrum)"
TL.Spectrum_short <- TL.Spectrum
TL.Spectrum_short@data <- TL.Spectrum_short@data[- 1, ]
TL.Spectrum_zeros <- TL.Spectrum
TL.Spectrum_zeros@data[10:12, 2] <- 0
TL.curve.1 <- get_RLum(OSL.SARMeasurement$Sequence.Object,
                       recordType = "TL (UVVIS)")[[1]]

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(merge_RLum.Data.Spectrum("error", merge.method = "/"),
               "'object' should be of class 'list'")
  expect_error(merge_RLum.Data.Spectrum(list("error")),
               "All elements of 'object' should be of class 'RLum.Data.Spectrum'")
  expect_error(merge_RLum.Data.Spectrum(list(), merge.method = "-"),
               "'object' contains no data")

  expect_error(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum),
                               merge.method = "error"),
               "'merge.method' should be one of 'mean', 'median', 'sum', 'sd'")
  expect_error(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum_types)),
               "Only similar record types are supported")

  expect_error(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum_short)),
               "'RLum.Data.Spectrum' objects of different size cannot be merged")
  TL.Spectrum_other <- TL.Spectrum
  rownames(TL.Spectrum_other@data) <- 1:nrow(TL.Spectrum_other@data)
  expect_error(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum_other)),
               "'RLum.Data.Spectrum' objects with different channels cannot")
  TL.Spectrum_other <- TL.Spectrum
  TL.Spectrum_other@info$cameraType <- "other"
  expect_error(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum_other)),
               "'RLum.Data.Spectrum' objects from different camera types")

  ## time/temperature differences
  TL.Spectrum_other <- TL.Spectrum
  colnames(TL.Spectrum_other@data) <- as.numeric(colnames(TL.Spectrum@data)) + 1
  expect_warning(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum_other)),
                 "The time/temperatures recorded are too different")
  expect_silent(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum_other),
                                         max.temp.diff = 1))
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  expect_snapshot_RLum(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum),
                                                method.info = 1))
  expect_snapshot_RLum(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum),
                                                merge.method = "sum"))
  expect_snapshot_RLum(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum),
                                                merge.method = "median"))

  expect_snapshot_RLum(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum),
                                                merge.method = "sd"))

  expect_snapshot_RLum(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum),
                                                merge.method = "var"))

  expect_snapshot_RLum(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum),
                                                merge.method = "max"))

  expect_snapshot_RLum(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum),
                                                merge.method = "min"))

  expect_snapshot_RLum(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum),
                                                merge.method = "-"))

  expect_s4_class(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum, TL.Spectrum),
                                           merge.method = "-"), "RLum.Data.Spectrum")

  expect_snapshot_RLum(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum),
                                                merge.method = "append"))

  expect_snapshot_RLum(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum),
                                                merge.method = "*"))

  expect_s4_class(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum, TL.Spectrum),
                                                merge.method = "*"), "RLum.Data.Spectrum")

  expect_snapshot_RLum(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum),
                                                merge.method = "/"))

  expect_s4_class(merge_RLum.Data.Spectrum(list(TL.Spectrum, TL.Spectrum, TL.Spectrum),
                                           merge.method = "/"), "RLum.Data.Spectrum")

  expect_warning(
      expect_snapshot_RLum(merge_RLum.Data.Spectrum(list(TL.Spectrum,
                                                         TL.Spectrum_zeros),
                                                    merge.method = "/")),
      "3 'inf' values have been replaced by 0 in the matrix")
})
