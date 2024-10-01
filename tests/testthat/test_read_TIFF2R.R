test_that("Test general functionality", {
  testthat::skip_on_cran()

  expect_error(read_TIFF2R(data.frame()),
               "'file' should be of class 'character'")
  expect_error(object = read_TIFF2R(file = "text"),
               "[read_TIFF2R()] File does not exist or is not readable",
               fixed = TRUE)

  ## test import
  file <- system.file("extdata", "TIFFfile.tif", package = "Luminescence")
  expect_s4_class(read_TIFF2R(file), "RLum.Data.Image")
})
