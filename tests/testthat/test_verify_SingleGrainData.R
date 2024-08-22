test_that("Various function test", {
  testthat::skip_on_cran()

  expect_error(verify_SingleGrainData("test"),
               "Input type 'character' is not allowed for this function")

  data(ExampleData.XSYG, envir = environment())
  object <- get_RLum(
    OSL.SARMeasurement$Sequence.Object, recordType = "OSL (UVVIS)", drop = FALSE)

  ## RLum.Analysis object
  expect_warning(output <- verify_SingleGrainData(object),
                 "selection_id is NA, nothing removed, everything selected")
  expect_s4_class(output, "RLum.Results")
  expect_s3_class(output$selection_full, "data.frame")
  expect_equal(sum(output@data$selection_full$VALID), 11)
  expect_equal(output@originator, "verify_SingleGrainData")

  expect_output(res <- verify_SingleGrainData(object, cleanup = TRUE,
                                              cleanup_level = "curve",
                                              threshold = 100),
                "RLum.Analysis object reduced to records")
  expect_s4_class(res, "RLum.Analysis")
  expect_equal(res@originator, "verify_SingleGrainData")
  expect_length(res@records, 5)

  ## threshold too high, empty object generated
  expect_output(res <- verify_SingleGrainData(object, cleanup = TRUE,
                                              cleanup_level = "curve",
                                              threshold = 2000),
                "RLum.Analysis object reduced to records")
  expect_s4_class(res, "RLum.Analysis")
  expect_equal(res@originator, "read_XSYG2R")
  expect_length(res@records, 0)

  ## Risoe.BINfileData
  data(ExampleData.BINfileData, envir = environment())
  res <- expect_silent(verify_SingleGrainData(CWOSL.SAR.Data))
  expect_s4_class(res, "RLum.Results")

  res <- expect_output(verify_SingleGrainData(CWOSL.SAR.Data, cleanup = TRUE),
                       "Risoe.BINfileData object reduced to records")
  expect_s4_class(res, "Risoe.BINfileData")

  obj.risoe <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)
  res <- expect_silent(verify_SingleGrainData(obj.risoe))
  expect_s4_class(res, "RLum.Results")

  ##check options
  expect_silent(suppressWarnings(verify_SingleGrainData(object, plot = TRUE)))
  expect_silent(suppressWarnings(verify_SingleGrainData(object, threshold = 100)))
  expect_silent(suppressWarnings(verify_SingleGrainData(object, verbose = FALSE)))
  expect_silent(suppressWarnings(verify_SingleGrainData(object, cleanup = TRUE)))
  expect_silent(verify_SingleGrainData(object, cleanup_level = "curve"))
  expect_silent(suppressWarnings(verify_SingleGrainData(list(object), cleanup = TRUE)))
  expect_silent(suppressWarnings(verify_SingleGrainData(list(object))))
})
