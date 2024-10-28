data(ExampleData.BINfileData, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(Risoe.BINfileData2RLum.Analysis("test"),
               "'object' should be of class 'Risoe.BINfileData")
  expect_error(Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = "test"),
               "'pos' should be of class 'numeric' or 'integer'")
  expect_error(Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, run = 10:12),
               "run = 10, 11, 12 contains invalid runs")
  expect_error(Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, set = 10:12),
               "set = 10, 11, 12 contains invalid sets")
  expect_error(Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, ltype = 10:12),
               "ltype = '10', '11', '12' contains invalid ltypes")
  expect_error(Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, dtype = 10:12),
               "dtype = '10', '11', '12' contains invalid dtypes")

  expect_warning(Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1:30),
                 "Invalid position number skipped")
  expect_warning(Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, grain = 1:10),
                 "Invalid grain number skipped")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  SW({
  res <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data,
                                         txtProgressBar = TRUE)
  })
  expect_type(res, "list")
  expect_length(res, 24)

  SW({
  res <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1:3,
                                         txtProgressBar = TRUE)
  })
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

  ## reading an empty object
  empty <- read_BIN2R(test_path("_data/BINfile_V3.bin"), n.records = 999,
                      verbose = FALSE)
  expect_null(Risoe.BINfileData2RLum.Analysis(empty))
  expect_null(Risoe.BINfileData2RLum.Analysis(empty, keep.empty = FALSE))
  expect_warning(expect_null(Risoe.BINfileData2RLum.Analysis(empty, pos = 0)),
                 "Invalid position number skipped: 0")

  ## reading an object with fields set to zero
  zero <- set_Risoe.BINfileData(METADATA = data.frame(ID = 1, VERSION = 0,
                                                      POSITION = 0, GRAIN = 0),
                                DATA = list(),
                                .RESERVED = list())
  expect_message(res <- Risoe.BINfileData2RLum.Analysis(zero),
                 "Empty Risoe.BINfileData object detected")
  expect_s4_class(res, "RLum.Analysis")
  expect_length(res, 0)
  expect_message(res <- Risoe.BINfileData2RLum.Analysis(zero, pos = 0),
                 "Empty Risoe.BINfileData object detected")
  expect_s4_class(res, "RLum.Analysis")
  expect_length(res, 0)
  expect_message(expect_null(
      Risoe.BINfileData2RLum.Analysis(zero, keep.empty = FALSE)),
      "Empty Risoe.BINfileData object detected")
})
