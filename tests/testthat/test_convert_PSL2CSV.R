context("convert_PSL2CSV()")

test_that("General test", {
  testthat::skip_on_cran()

  ##get file
  file <- system.file("extdata/DorNie_0016.psl", package="Luminescence")

  ##stop
  expect_error(convert_PSL2CSV())

  ##the case where we have an object of type RLum
  expect_is(convert_PSL2CSV(read_PSL2R(file), export = FALSE), class = "list")

  ##export FALSE
  expect_is(convert_PSL2CSV(file, export = FALSE), class = "list")

  ##write to temp
  expect_silent(convert_PSL2CSV(file, export = TRUE, path = tempdir()))


})

