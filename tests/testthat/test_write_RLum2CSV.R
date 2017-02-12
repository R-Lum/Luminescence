context("write_RLumCSV")

test_that("test errors and general export function", {
  testthat::skip_on_cran()

  ##test error
  expect_error(write_RLum2CSV(object = "", export = FALSE),
               regexp = "[write_RLum2CSV()] Object needs to be a member of the object class RLum!",
               fixed = TRUE)

  ##test export
  data("ExampleData.portableOSL", envir = environment())
  expect_is(write_RLum2CSV(ExampleData.portableOSL, export = FALSE), "list")

})
