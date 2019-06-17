context("read_SPE2R")

test_that("Test general functionality", {
  testthat::skip_on_cran()

  ##crash function
  expect_null(read_SPE2R(file = "text"))

  ## default values
  expect_is(read_SPE2R("https://github.com/R-Lum/Luminescence/blob/master/tests/testdata/SPEfile.SPE?raw=true"), "RLum.Data.Image")

  ##test verbose
  expect_is(read_SPE2R("https://github.com/R-Lum/Luminescence/blob/master/tests/testdata/SPEfile.SPE?raw=true", verbose = FALSE), "RLum.Data.Image")

})
