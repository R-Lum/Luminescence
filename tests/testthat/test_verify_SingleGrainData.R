## load data
data(ExampleData.XSYG, envir = environment())
object <- get_RLum(OSL.SARMeasurement$Sequence.Object,
                   recordType = "OSL (UVVIS)", drop = FALSE)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(verify_SingleGrainData("test"),
               "'object' should be of class 'Risoe.BINfileData', 'RLum.Analysis'")
  expect_error(verify_SingleGrainData(object, threshold = "error"),
               "'threshold' should be of class 'numeric' or 'integer'")
  expect_error(verify_SingleGrainData(object, use_fft = "error"),
               "'use_fft' should be a single logical value")
  expect_error(verify_SingleGrainData(object, cleanup = "error"),
               "'cleanup' should be a single logical value")
  expect_error(verify_SingleGrainData(object, cleanup_level = "error"),
               "'cleanup_level' should be one of 'aliquot' or 'curve'")

  object@originator <- "error"
  expect_error(verify_SingleGrainData(object),
               "Object originator 'error' not supported")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## RLum.Analysis object
  expect_warning(output <- verify_SingleGrainData(object),
                 "'selection_id' is NA, everything tagged for removal")
  expect_s4_class(output, "RLum.Results")
  expect_s3_class(output$selection_full, "data.frame")
  expect_equal(sum(output@data$selection_full$VALID), 11)
  expect_equal(output@originator, "verify_SingleGrainData")

  expect_message(res <- verify_SingleGrainData(object, cleanup = TRUE,
                                              cleanup_level = "curve",
                                              threshold = 100),
                "RLum.Analysis object reduced to records")
  expect_s4_class(res, "RLum.Analysis")
  expect_equal(res@originator, "verify_SingleGrainData")
  expect_length(res@records, 5)

  ## threshold too high, empty object generated
  expect_message(res <- verify_SingleGrainData(object, cleanup = TRUE,
                                              cleanup_level = "curve",
                                              threshold = 2000),
                "RLum.Analysis object reduced to records")
  expect_s4_class(res, "RLum.Analysis")
  expect_equal(res@originator, "read_XSYG2R")
  expect_length(res@records, 0)

  ## threshold too high on a list
  expect_message(res <- verify_SingleGrainData(list(object), cleanup = TRUE,
                                               cleanup_level = "curve",
                                               threshold = 2000),
                 "RLum.Analysis object reduced to records")
  expect_type(res, "list")
  expect_s4_class(res[[1]], "RLum.Analysis")
  expect_equal(res[[1]]@originator, "read_XSYG2R")
  expect_length(res[[1]]@records, 0)

  ## check for cleanup
  data(ExampleData.BINfileData, envir = environment())
  t <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data)
  expect_warning(
    object = verify_SingleGrainData(t, cleanup = TRUE, threshold = 20000),
    regexp = "Verification and cleanup removed all records, NULL returned")
  expect_null(suppressWarnings(verify_SingleGrainData(t, cleanup = TRUE, threshold = 20000)))

  ## Risoe.BINfileData
  res <- expect_silent(verify_SingleGrainData(CWOSL.SAR.Data, plot = TRUE))
  expect_s4_class(res, "RLum.Results")

  res <- expect_output(verify_SingleGrainData(CWOSL.SAR.Data, cleanup = TRUE,
                                              cleanup_level = "curve"),
                       "Risoe.BINfileData object reduced to records")
  expect_s4_class(res, "Risoe.BINfileData")

  ## use_fft
  expect_s4_class(verify_SingleGrainData(CWOSL.SAR.Data, cleanup = TRUE, use_fft = TRUE, verbose = FALSE),
                "Risoe.BINfileData")

  obj.risoe <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)
  res <- expect_silent(verify_SingleGrainData(obj.risoe))
  expect_s4_class(verify_SingleGrainData(obj.risoe, use_fft = TRUE), "RLum.Results")

  ## remove all and cleanup
  expect_warning(
    object = verify_SingleGrainData(CWOSL.SAR.Data, cleanup = TRUE, threshold = 20000),
    regexp = "Verification and cleanup removed all records, NULL returned")
  expect_null(suppressWarnings(verify_SingleGrainData(CWOSL.SAR.Data, cleanup = TRUE, threshold = 20000)))

  ## empty list
  expect_s4_class(res <- verify_SingleGrainData(list()),
                  "RLum.Results")
  expect_length(res@data, 0)
  expect_equal(res@originator, "verify_SingleGrainData")

  expect_s4_class(res <- verify_SingleGrainData(list(), cleanup = TRUE),
                  "RLum.Analysis")
  expect_length(res@records, 0)
  expect_equal(res@originator, "verify_SingleGrainData")

  ## list
  expect_silent(suppressWarnings(verify_SingleGrainData(list(object))))
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 740
  object@records[[1]]@info$position <- "123"
  expect_s4_class(verify_SingleGrainData(object),
                  "RLum.Results")
})
