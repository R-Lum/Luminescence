## load datax
data(ExampleData.BINfileData, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(convert_BIN2CSV(),
               "'file' should be of class 'character' or 'Risoe.BINfileData'")
  expect_error(convert_BIN2CSV(character(0)),
               "'file' cannot be an empty character")
  expect_error(convert_BIN2CSV(file = "error", export = FALSE),
               "File '.*error' does not exist") # windows CI needs the regexp
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_type(convert_BIN2CSV(subset(CWOSL.SAR.Data, POSITION == 1),
                              export = FALSE),
              "list")
  expect_null(convert_BIN2CSV(subset(CWOSL.SAR.Data, POSITION == 1),
                              export = TRUE, path = tempdir()))
})
