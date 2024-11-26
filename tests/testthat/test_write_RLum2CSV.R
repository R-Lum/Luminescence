## load data
data(ExampleData.portableOSL, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(write_RLum2CSV(object = "", export = FALSE),
               "[write_RLum2CSV()] 'object' should be of class 'RLum.Analysis'",
               fixed = TRUE)
  expect_error(write_RLum2CSV(ExampleData.portableOSL[[1]], export = TRUE,
                              path = "non-existing"),
               "Directory provided via the argument 'path' does not exist")
  expect_error(write_RLum2CSV(set_RLum("RLum.Results"), verbose = FALSE),
               "'object' cannot be an empty RLum.Results")
})

test_that("check functionality", {
  testthat::skip_on_cran()

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
                 "elements could not be converted to CSV")

  ##using option compact = FALSE
  expect_warning(write_RLum2CSV(object = results, export = FALSE,
                                compact = FALSE),
                 "elements could not be converted to CSV")
  expect_warning(write_RLum2CSV(object = results,export = FALSE, compact = TRUE),
                 "elements could not be converted to CSV")

  ## no valid records
  res.invalid <- results
  res.invalid@data$summary <- res.invalid@data$data <- NULL
  res.invalid@data$args$sigmab <- NULL
  expect_warning(expect_error(write_RLum2CSV(res.invalid, export = FALSE)),
                 "elements could not be converted to CSV")

  ##real export
  expect_warning(
    write_RLum2CSV(object = results, path = tempdir(), compact = TRUE),
    "elements could not be converted to CSV")

  ## data.frame
  df <- results@data$data
  expect_null(write_RLum2CSV(object = df, path = tempdir()))
  attr(df, "filename") <- "test"
  expect_null(write_RLum2CSV(object = df, path = tempdir()))

  ## empty list
  expect_null(write_RLum2CSV(list()))
})
