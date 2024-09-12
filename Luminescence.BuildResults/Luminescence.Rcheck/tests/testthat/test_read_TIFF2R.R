test_that("Test general functionality", {
  testthat::skip_on_cran()

  ##crash function
  expect_error(object = read_TIFF2R(file = "text"),
               regexp = "\\[read_TIFF2R\\(\\)\\] File does not exist or is not readable!")

  ## test import
  file <- system.file("extdata", "TIFFfile.tif", package = "Luminescence")
  expect_s4_class(read_TIFF2R(file), "RLum.Data.Image")


})
