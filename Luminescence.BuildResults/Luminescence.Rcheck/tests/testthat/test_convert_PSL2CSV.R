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


})

