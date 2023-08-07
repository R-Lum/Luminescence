test_that("Various function test", {
  testthat::skip_on_cran()
  local_edition(3)

  data(ExampleData.XSYG, envir = environment())
  object <- get_RLum(
    OSL.SARMeasurement$Sequence.Object, recordType = "OSL (UVVIS)", drop = FALSE)

  ##initial
  expect_warning(verify_SingleGrainData(object))
  output <- suppressWarnings(verify_SingleGrainData(object))

  ##return value
  expect_s4_class(output, "RLum.Results")
  expect_s3_class(output$selection_full, "data.frame")

  ##check options
  expect_silent(suppressWarnings(verify_SingleGrainData(object, plot = TRUE)))
  expect_silent(suppressWarnings(verify_SingleGrainData(object, threshold = 100)))
  expect_silent(suppressWarnings(verify_SingleGrainData(object, verbose = FALSE)))
  expect_silent(suppressWarnings(verify_SingleGrainData(object, cleanup = TRUE)))

})

