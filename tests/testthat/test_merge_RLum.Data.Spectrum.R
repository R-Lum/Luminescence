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

  expect_error(merge_RLum("error", merge.method = "/"),
               "'object' should be of class 'list'")
  expect_error(merge_RLum(list(TL.Spectrum, TL.Spectrum),
                               merge.method = "error"),
               "'merge.method' should be one of 'mean', 'median', 'sum', 'sd'")
  expect_error(merge_RLum(list(TL.Spectrum, TL.Spectrum),
                               method.info = "error"),
               "'method.info' should be a single positive integer value or NULL")
  expect_error(merge_RLum(list(TL.Spectrum, TL.Spectrum),
                                        method.info = 10),
               "'method.info' cannot exceed the number of objects being merged")
  expect_error(merge_RLum(list(set_RLum("RLum.Data.Spectrum"))),
               "'object' contains no data")
  expect_error(merge_RLum(list(set_RLum("RLum.Data.Spectrum", data = matrix(1)),
                               set_RLum("RLum.Data.Spectrum", data = matrix(2)))),
               "'object' contains no data")
  expect_error(merge_RLum(list(TL.Spectrum, TL.Spectrum_types)),
               "Objects cannot be merged, different record types found")
  expect_error(merge_RLum(list(TL.Spectrum, TL.Spectrum_short)),
               "'RLum.Data.Spectrum' objects of different size cannot be merged")
  TL.Spectrum_other <- TL.Spectrum
  rownames(TL.Spectrum_other@data) <- 1:nrow(TL.Spectrum_other@data)
  expect_error(merge_RLum(list(TL.Spectrum, TL.Spectrum_other)),
               "'RLum.Data.Spectrum' objects with different channels cannot")
  TL.Spectrum_other <- TL.Spectrum
  TL.Spectrum_other@info$cameraType <- "other"
  expect_error(merge_RLum(list(TL.Spectrum, TL.Spectrum_other)),
               "'RLum.Data.Spectrum' objects from different camera types")

  ## time/temperature differences
  TL.Spectrum_other <- TL.Spectrum
  colnames(TL.Spectrum_other@data) <- as.numeric(colnames(TL.Spectrum@data)) + 1
  expect_warning(merge_RLum(list(TL.Spectrum, TL.Spectrum_other)),
                 "The time/temperatures recorded are too different")
  expect_silent(merge_RLum(list(TL.Spectrum, TL.Spectrum_other),
                                         max.temp.diff = 1))
  spectrum <- set_RLum("RLum.Data.Spectrum", data = matrix(1:10, ncol = 2))
  expect_no_warning(merge_RLum(list(spectrum, spectrum)))
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expected <- TL.Spectrum@data
  zeros <- array(0, dim(expected), dimnames = dimnames(expected))

  ## only one spectrum
  expect_s4_class(merged <- merge_RLum(list(TL.Spectrum)),
                  "RLum.Data.Spectrum")
  expect_equal(merged@data,
               expected)

  ## two spectra
  objects <- list(TL.Spectrum, TL.Spectrum)
  expect_equal(merge_RLum(objects, merge.method = "-")@data,
               zeros)
  expect_equal(merge_RLum(objects, merge.method = "*")@data,
               expected^2)
  expect_equal(merge_RLum(objects, merge.method = "/")@data,
               zeros + 1)

  ## more than two spectra
  objects <- list(TL.Spectrum, TL.Spectrum, TL.Spectrum)
  expect_equal(merge_RLum(objects, merge.method = "-")@data,
               -expected)
  expect_equal(merge_RLum(objects, merge.method = "*")@data,
               2 * expected^2)
  expect_equal(merge_RLum(objects, merge.method = "/")@data,
               zeros + 0.5)

  ## single-row spectrum
  data <- matrix(1:4, nrow = 1)
  spectrum <- set_RLum("RLum.Data.Spectrum",
                       data = data)
  expect_equal(merge_RLum(list(spectrum, spectrum))@data,
               data)
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  expect_snapshot_RLum(merge_RLum(list(TL.Spectrum, TL.Spectrum),
                                                method.info = 1))
  expect_snapshot_RLum(merge_RLum(list(TL.Spectrum, TL.Spectrum),
                                                merge.method = "sum"))
  expect_snapshot_RLum(merge_RLum(list(TL.Spectrum, TL.Spectrum),
                                                merge.method = "median"))

  expect_snapshot_RLum(merge_RLum(list(TL.Spectrum, TL.Spectrum),
                                                merge.method = "sd"))

  expect_snapshot_RLum(merge_RLum(list(TL.Spectrum, TL.Spectrum),
                                                merge.method = "var"))

  expect_snapshot_RLum(merge_RLum(list(TL.Spectrum, TL.Spectrum),
                                                merge.method = "max"))

  expect_snapshot_RLum(merge_RLum(list(TL.Spectrum, TL.Spectrum),
                                                merge.method = "min"))

  expect_snapshot_RLum(merge_RLum(list(TL.Spectrum, TL.Spectrum),
                                                merge.method = "append"))

  expect_warning(
      expect_snapshot_RLum(merge_RLum(list(TL.Spectrum,
                                                         TL.Spectrum_zeros),
                                                    merge.method = "/")),
      "3 Inf values replaced by 0 in the matrix")
})
