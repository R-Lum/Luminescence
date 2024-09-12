test_that("test errors and general export function", {
  testthat::skip_on_cran()

  ##test error
  expect_error(write_RLum2CSV(),
               "input object is missing")
  expect_error(write_RLum2CSV(object = "", export = FALSE),
               regexp = "[write_RLum2CSV()] Object needs to be a member of the object class RLum!",
               fixed = TRUE)

  ##test export
  data("ExampleData.portableOSL", envir = environment())
  expect_error(write_RLum2CSV(ExampleData.portableOSL[[1]], export = TRUE,
                              path = "non-existing"),
               "Directory provided via the argument 'path' does not exist")

  ## move temporarily to avoid polluting the working directory
  cwd <- setwd(tempdir())
  expect_message(
      expect_null(write_RLum2CSV(ExampleData.portableOSL[[1]], export = TRUE)),
    "Path automatically set to")
  expect_type(write_RLum2CSV(ExampleData.portableOSL, export = FALSE), "list")
  setwd(cwd)

  data("ExampleData.RLum.Data.Image", envir = environment())
  write_RLum2CSV(ExampleData.RLum.Data.Image, path = tempdir())

  ##test RLum.Results objects
  ## load example data
  data(ExampleData.DeValues, envir = environment())
  SW({
  results <-  calc_CommonDose(ExampleData.DeValues$CA1)
  })

  ##using option compact
  expect_warning(write_RLum2CSV(object = results,export = FALSE),
                 regexp = "elements could not be converted to a CSV-structure!")

  ##using option compact = FALSE
  expect_warning(write_RLum2CSV(object = results, export = FALSE,
                                compact = FALSE),
                 "elements could not be converted to a CSV-structure")
  expect_warning(write_RLum2CSV(object = results,export = FALSE, compact = TRUE),
                 regexp = "elements could not be converted to a CSV-structure!")

  ##real export
  expect_warning(
    write_RLum2CSV(object = results, path = tempdir(), compact = TRUE),
    regexp = "elements could not be converted to a CSV-structure!")

  ## data.frame
  df <- results@data$data
  expect_null(write_RLum2CSV(object = df, path = tempdir()))
  attr(df, "filename") <- "test"
  expect_null(write_RLum2CSV(object = df, path = tempdir()))
})
