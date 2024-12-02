## load data
data(ExampleData.DeValues, envir = environment())

results <- convert_Second2Gray(ExampleData.DeValues$BT998, c(0.2,0.01))
dose.rate <- calc_SourceDoseRate(measurement.date = "2024-10-09",
                                 calib.date = "2014-12-19",
                                 calib.dose.rate = 0.2, calib.error = 0.01)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(convert_Second2Gray("test"),
               "'data' should be of class 'data.frame'")
  expect_error(convert_Second2Gray(ExampleData.DeValues$BT998, dose.rate = FALSE),
               "'dose.rate' should be of class 'RLum.Results', 'data.frame' or")
  expect_error(convert_Second2Gray(ExampleData.DeValues$BT998,
                           dose.rate = results[1:5, ]),
               "'data' and 'dose.rate' must have the same length")
  expect_error(convert_Second2Gray(ExampleData.DeValues$BT998,
                           dose.rate = results,
                           error.propagation = "test"),
               "'error.propagation' should be one of 'omit', 'gaussian'")

  dose.rate@originator <- "unexpected-originator"
  expect_error(convert_Second2Gray(ExampleData.DeValues$BT998, dose.rate = dose.rate),
               "Wrong originator for dose.rate 'RLum.Results' object")

  ## defunct name
  expect_error(Second2Gray(ExampleData.DeValues$BT998, dose.rate = dose.rate),
               "'Second2Gray' is defunct")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  expect_snapshot_plain(results)
  expect_snapshot_plain(convert_Second2Gray(ExampleData.DeValues$BT998,
                                    dose.rate = c(0.2, 0.01),
                                    error.propagation = "gaussian"))
  expect_snapshot_plain(convert_Second2Gray(ExampleData.DeValues$BT998,
                                    dose.rate = c(0.2, 0.01),
                                    error.propagation = "absolute"))

  expect_snapshot_plain(convert_Second2Gray(ExampleData.DeValues$BT998,
                                    dose.rate = results,
                                    error.propagation = "gaussian"))
  expect_snapshot_plain(convert_Second2Gray(ExampleData.DeValues$BT998,
                                    dose.rate = results,
                                    error.propagation = "absolute"))

  expect_snapshot_plain(convert_Second2Gray(ExampleData.DeValues$BT998,
                                    dose.rate = dose.rate))
})
