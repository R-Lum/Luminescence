test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(convert_Daybreak2CSV(),
               "'file' should be of class 'character' or 'RLum'")
  expect_error(convert_Daybreak2CSV(character(0)),
               "'file' cannot be an empty character")
  expect_error(convert_Daybreak2CSV(""),
               "File does not exist")
})

test_that("check class and length of output", {
  testthat::skip_on_cran()

  SW({
  res <- read_Daybreak2R(file = system.file("extdata/Daybreak_TestFile.txt",
                                            package = "Luminescence"))[[1]]
  })
  expect_null(convert_Daybreak2CSV(res, path = tempdir()))
  expect_type(convert_Daybreak2CSV(res, path = tempdir(), export = FALSE),
              "list")
})
