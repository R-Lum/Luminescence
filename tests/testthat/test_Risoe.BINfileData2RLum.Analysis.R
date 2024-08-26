data(ExampleData.BINfileData, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(Risoe.BINfileData2RLum.Analysis("test"),
               "Input object is not of type 'Risoe.BINfileData")
  expect_error(Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = "test"),
               "'pos' has to be of type numeric")
  expect_error(Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, run = 10:12),
               "run = 10,11,12 contains invalid runs")
  expect_error(Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, set = 10:12),
               "set = 10,11,12 contains invalid sets")
  expect_error(Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, ltype = 10:12),
               "ltype = 10,11,12 contains invalid ltypes")
  expect_error(Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, dtype = 10:12),
               "dtype = 10,11,12 contains invalid dtypes")

  expect_warning(Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1:30),
                 "Invalid position number skipped")
  expect_warning(Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, grain = 1:10),
                 "Invalid grain number skipped")
})

test_that("input validation", {
  testthat::skip_on_cran()

  res <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data,
                                         txtProgressBar = TRUE)
  expect_type(res, "list")
  expect_length(res, 24)

  res <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1:3,
                                         txtProgressBar = TRUE)
  expect_type(res, "list")
  expect_length(res, 3)

  res <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1,
                                         txtProgressBar = TRUE)
  expect_s4_class(res, "RLum.Analysis")

  ## FI readers like to write a NA instead of 0 in the grain column
  obj <- CWOSL.SAR.Data
  obj@METADATA[["GRAIN"]] <- rep(NA, length(obj@METADATA[["GRAIN"]]))
  res <- Risoe.BINfileData2RLum.Analysis(obj, pos = 1)
  expect_s4_class(res, "RLum.Analysis")
})
