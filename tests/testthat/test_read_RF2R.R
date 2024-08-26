test_that("Test functionality", {
  testthat::skip_on_cran()

  ##load file path
  file <- system.file("extdata", "RF_file.rf", package = "Luminescence")

  ##crash function
  expect_error(read_RF2R("file"), regexp = "File 'file' does not exist!")
  expect_error(read_RF2R(2), regexp = "'file' needs to be of type character!")

  ##simple import
  expect_type(read_RF2R(file), type = "list")

  ##import list
  expect_type(expect_message(read_RF2R(list(file, "test")),
                             "Error: Import for file 'test' failed"),
              type = "list")

  ##import false list
  expect_warning(read_RF2R(c(file, file)), regexp = "'file' has a length > 1. Only the first element was taken!")

  ## create a file with unsupported version
  file.wrong <- "RF_wrong_version.Rf"
  writeLines(gsub("17-10-2018", "wrong-version", readLines(file)),
             file.wrong)
  expect_error(read_RF2R(file.wrong),
               "File format not supported")
  file.remove(file.wrong)

  ## create a file with malformed header
  file.wrong <- "RF_wrong_header.Rf"
  writeLines(gsub("grain_d=20", "grain_d=", readLines(file)),
             file.wrong)
  expect_message(read_RF2R(file.wrong),
                 "Error: Header extraction failed")
  file.remove(file.wrong)
})
