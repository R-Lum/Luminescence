test_that("test errors and general export function", {
  testthat::skip_on_cran()
  local_edition(3)

  ##test error
  expect_error(write_RLum2CSV(object = "", export = FALSE),
               regexp = "[write_RLum2CSV()] Object needs to be a member of the object class RLum!",
               fixed = TRUE)

  ##test export
  data("ExampleData.portableOSL", envir = environment())
  expect_type(write_RLum2CSV(ExampleData.portableOSL, export = FALSE), "list")

  ##test RLum.Results objects
  ## load example data
  data(ExampleData.DeValues, envir = environment())
  results <-  calc_CommonDose(ExampleData.DeValues$CA1)

  ##using option compact
  expect_warning(write_RLum2CSV(object = results,export = FALSE),
                 regexp = "elements could not be converted to a CSV-structure!")

  ##using option compact = FALSE
  expect_warning(write_RLum2CSV(object = results,export = FALSE, compact = TRUE),
                 regexp = "elements could not be converted to a CSV-structure!")

  ##real export
  expect_warning(
    write_RLum2CSV(object = results, path = tempdir(), compact = TRUE),
    regexp = "elements could not be converted to a CSV-structure!")


})
