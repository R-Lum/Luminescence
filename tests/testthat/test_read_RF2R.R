context("read_RF2R")

test_that("Test functionality", {
  testthat::skip_on_cran()

  ##load file path
  file <- system.file("extdata", "RF_file.rf", package = "Luminescence")

  ##crash function
  expect_error(read_RF2R("file"), regexp = "File 'file' does not exist!")
  expect_error(read_RF2R("file"), regexp = "'file' needs to be of type character!")

  ##simple import
  expect_type(read_RF2R(file), type = "list")

  ##import list
  expect_warning(read_RF2R(list(file, "test")), regexp = "Import for file test failed. NULL returned!")


})
