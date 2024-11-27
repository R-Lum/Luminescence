test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(convert_PSL2CSV(),
               "'file' should be of class 'character' or 'RLum'")
  expect_error(convert_PSL2CSV(character(0)),
               "'file' cannot be an empty character")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## get file
  file <- system.file("extdata/DorNie_0016.psl", package = "Luminescence")

  ##the case where we have an object of type RLum
  expect_type(convert_PSL2CSV(read_PSL2R(file, verbose = FALSE), export = FALSE),
              "list")

  ##export FALSE
  expect_type(convert_PSL2CSV(file, export = FALSE, verbose = FALSE),
              "list")

  ##write to temp
  expect_silent(convert_PSL2CSV(file, export = TRUE, path = tempdir(),
                                verbose = FALSE))

  ##test single_table argument
  expect_type(convert_PSL2CSV(file, export = FALSE, single_table = TRUE,
                              verbose = FALSE),
              "list")

  ##test raw data extraction
  ## simple raw data extraction
  t <- expect_type(convert_PSL2CSV(file, export = FALSE,
                                   extract_raw_data = TRUE, verbose = FALSE),
                   "list")
  expect_length(t, 5)

  ## raw data extraction with single_table
  t <- expect_type(convert_PSL2CSV(file, export = FALSE, single_table = TRUE,
                                   extract_raw_data = TRUE, verbose = FALSE),
                   "list")
  expect_length(t, 1)
  expect_equal(nrow(t[[1]]), 100)

  ## test with files export
  tmp_path <- tempdir()
  expect_silent(convert_PSL2CSV(file, path = tmp_path, extract_raw_data = TRUE,
                                single_table = TRUE, col.names = TRUE,
                                verbose = FALSE))

  ## test with col.names
  df <- read.table(file = rev(list.files(path = tmp_path, pattern = ".csv", full.names = TRUE))[1], sep = ";", header = TRUE)
  expect_type(colnames(df), "character")
  expect_true(grepl(pattern = "USER", colnames(df)[1]))

  ## test without column names
  expect_silent(convert_PSL2CSV(file, path = tmp_path,
                                extract_raw_data = TRUE, single_table = TRUE,
                                col.names = FALSE, verbose = FALSE))
  df <- read.table(file = list.files(path = tmp_path, pattern = ".csv", full.names = TRUE)[1], sep = ";", header = TRUE)
  expect_false(grepl(pattern = "USER", colnames(df)[1]))
})
