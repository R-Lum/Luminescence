## load data
file <- system.file("extdata/NCF.binx", package = "Luminescence")
ncf <- read_BIN2R(file, fastForward = TRUE, verbose = FALSE)

test_that("input validation", {
  testthat::skip_on_cran()

  data(ExampleData.BINfileData, envir = environment())
  object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1:2)
  expect_error(analyse_SAR.NCF(object, signal_integral = 1:2,
                               background_integral = 100:250),
               "No additional dose found, check that the NCF-SAR protocol was")


  expect_message(expect_message(expect_null(
      analyse_SAR.NCF(set_RLum("RLum.Analysis",
                               records = list(set_RLum("RLum.Data.Curve",
                                                       recordType = "false",
                                                       info = list(IRR_TIME = 10)))),
                      signal_integral = 1:2, background_integral = 100:200)),
      "Error: No record of type 'OSL', 'IRSL', 'POSL' detected, NULL returned"),
      "Error: CWOSL analysis skipped: check your sequence, NULL returned")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  SW({
  expect_warning(res <- analyse_SAR.NCF(ncf, signal_integral = 1:2,
                                        background_integral = 100:250,
                                        dose_rate_source = 4.702),
                 "Multiple IRSL curves detected")
  })
  expect_type(res, "list")
  expect_length(res, 1)
  expect_s4_class(res[[1]], "RLum.Results")

  expect_silent(analyse_SAR.NCF(ncf, signal_integral = 1:2,
                                background_integral = 100:250,
                                plot = FALSE, verbose = FALSE))
})
