test_that("Test functionality", {
  testthat::skip_on_cran()

  ##load file path
  file <- system.file("extdata", "RF_file.rf", package = "Luminescence")

  ##crash function
  expect_error(read_RF2R("error"),
               "File 'error' does not exist")
  expect_error(read_RF2R(2),
               "'file' should be of class 'character' or 'list'")

  ##simple import
  SW({
  expect_type(read_RF2R(file), type = "list")
  })

  ##import list
  expect_type(expect_message(read_RF2R(list(file, "test"), verbose = FALSE),
                             "Error: Import for file 'test' failed"),
              type = "list")

  ##import false list
  expect_warning(read_RF2R(c(file, file), verbose = FALSE),
                 "'file' has length > 1, only the first element was taken")

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
  expect_message(read_RF2R(file.wrong, verbose = FALSE),
                 "Error: Header extraction failed")
  file.remove(file.wrong)
})
