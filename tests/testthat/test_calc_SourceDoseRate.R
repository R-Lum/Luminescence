temp <- calc_SourceDoseRate(measurement.date = "2012-01-27",
                           calib.date = "2014-12-19",
                           calib.dose.rate = 0.0438,
                           calib.error = 0.0019)


test_that("General tests", {
  testthat::skip_on_cran()
  local_edition(3)

  ##simple run
  expect_silent(calc_SourceDoseRate(
    calib.date = "2014-12-19",
    calib.dose.rate = 0.0438,
    calib.error = 0.0019
  ))

  ##simple run
  expect_silent(calc_SourceDoseRate(
    measurement.date = "2018-01-02",
    calib.date = "2014-12-19",
    calib.dose.rate = 0.0438,
    calib.error = 0.0019
  ))

  ##simple run predict
  expect_silent(calc_SourceDoseRate(
    measurement.date = "2018-01-02",
    calib.date = "2014-12-19",
    calib.dose.rate = 0.0438,
    calib.error = 0.0019,
    predict = 10
  ))

  ##Gy/min as unit
  expect_silent(calc_SourceDoseRate(
    measurement.date = "2018-01-02",
    calib.date = "2014-12-19",
    calib.dose.rate = 0.0438,
    calib.error = 0.0019, dose.rate.unit = "Gy/min"
  ))


  ##cause stop
  expect_error(calc_SourceDoseRate(
    measurement.date = "2018-01-02",
    calib.date = "2014-12-19",
    calib.dose.rate = 0.0438,
    calib.error = 0.0019, source.type = "SK"
  ))



})

test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 3)

})

test_that("check values from output example 1", {
  testthat::skip_on_cran()
  local_edition(3)

  results <- get_RLum(temp)

  expect_equal(round(results$dose.rate, digits = 8), 0.04695031)
  expect_equal(round(results$dose.rate.error, digits = 9), 0.002036657)
  expect_equal(results$date, as.Date("2012-01-27"))

})
