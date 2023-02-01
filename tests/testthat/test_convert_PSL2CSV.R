test_that("General test", {
  testthat::skip_on_cran()
  local_edition(3)

  ##get file
  file <- system.file("extdata/DorNie_0016.psl", package="Luminescence")

  ##stop
  expect_error(convert_PSL2CSV())

  ##the case where we have an object of type RLum
  expect_type(convert_PSL2CSV(read_PSL2R(file), export = FALSE), "list")

  ##export FALSE
  expect_type(convert_PSL2CSV(file, export = FALSE), "list")

  ##write to temp
  expect_silent(convert_PSL2CSV(file, export = TRUE, path = tempdir()))

  ##test single_table argument
  expect_type(convert_PSL2CSV(file, export = FALSE, single_table = TRUE), "list")

  ##test raw data extraction
  ## simple raw data extraction
  t <- expect_type(convert_PSL2CSV(file, export = FALSE, extract_raw_data = TRUE), "list")
  expect_length(t, 5)

  ## raw data extraction with single_table
  t <- expect_type(convert_PSL2CSV(file, export = FALSE, extract_raw_data = TRUE, single_table = TRUE), "list")
  expect_length(t, 1)
  expect_equal(nrow(t[[1]]), 100)


})

