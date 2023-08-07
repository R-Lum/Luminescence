test_that("Test functionality", {
  testthat::skip_on_cran()
  local_edition(3)

  ##load file path
  file <- system.file("extdata", "RF_file.rf", package = "Luminescence")

  ##crash function
  expect_error(read_RF2R("file"), regexp = "File 'file' does not exist!")
  expect_error(read_RF2R(2), regexp = "'file' needs to be of type character!")

  ##simple import
  expect_type(read_RF2R(file), type = "list")

  ##import list
  expect_type(read_RF2R(list(file, "test")), type = "list")

  ##import false list
  expect_warning(read_RF2R(c(file, file)), regexp = "'file' has a length > 1. Only the first element was taken!")

})
