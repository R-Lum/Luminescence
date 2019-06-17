context("Test verify_SingleGrainData")

test_that("Various function test", {
  testthat::skip_on_cran()

  data(ExampleData.XSYG, envir = environment())
  object <- get_RLum(
    OSL.SARMeasurement$Sequence.Object, recordType = "OSL (UVVIS)", drop = FALSE)

  ##initial
  output <- verify_SingleGrainData(object)

  ##return value
  expect_is(output, "RLum.Results")
  expect_is(output$selection_full, "data.frame")

  ##check options
  expect_silent(suppressWarnings(verify_SingleGrainData(object, plot = TRUE)))
  expect_silent(suppressWarnings(verify_SingleGrainData(object, threshold = 100)))
  expect_silent(suppressWarnings(verify_SingleGrainData(object, verbose = FALSE)))
  expect_silent(suppressWarnings(verify_SingleGrainData(object, cleanup = TRUE)))

})

