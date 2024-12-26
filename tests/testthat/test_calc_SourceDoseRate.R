temp <- calc_SourceDoseRate(measurement.date = "2012-01-27",
                           calib.date = "2014-12-19",
                           calib.dose.rate = 0.0438,
                           calib.error = 0.0019)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_SourceDoseRate(data.frame()),
               "'measurement.date' should be of class 'Date' or 'character'")
  expect_error(calc_SourceDoseRate("2024-10-01", data.frame()),
               "'calib.date' should be of class 'Date' or 'character'")
})

test_that("General tests", {
  testthat::skip_on_cran()

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

  ## produce more than 100 rows
  t <- expect_silent(calc_SourceDoseRate(
    measurement.date = "2018-01-02",
    calib.date = "2014-12-19",
    calib.dose.rate = 0.0438,
    calib.error = 0.0019,
    predict = 150
  ))
  expect_silent(plot_RLum.Results(t))

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
    calib.error = 0.0019, source.type = "error"),
    "'source.type' should be one of 'Sr-90', 'Am-214', 'Co-60' or 'Cs-137'")

  expect_error(calc_SourceDoseRate(
    measurement.date = "2018-01-02",
    calib.date = "2014-12-19",
    calib.dose.rate = 0.0438,
    calib.error = 0.0019, dose.rate.unit = "error"),
    "'dose.rate.unit' should be one of 'Gy/s' or 'Gy.min'")
})

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
