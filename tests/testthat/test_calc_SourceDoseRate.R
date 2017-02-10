context("calc_SourceDoseRate")

temp <- calc_SourceDoseRate(measurement.date = "2012-01-27",
                           calib.date = "2014-12-19",
                           calib.dose.rate = 0.0438,
                           calib.error = 0.0019)

test_that("check class and length of output", {
  testthat::skip_on_cran()
  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 3)

})

test_that("check values from output example 1", {
  testthat::skip_on_cran()
  results <- get_RLum(temp)

  expect_equal(round(results$dose.rate, digits = 8), 0.04695031)
  expect_equal(round(results$dose.rate.error, digits = 9), 0.002036657)
  expect_equal(results$date, as.Date("2012-01-27"))

})
