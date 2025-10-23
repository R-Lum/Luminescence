test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_SourceDoseRate(data.frame()),
               "'measurement.date' should be of class 'Date' or 'character'")
  expect_error(calc_SourceDoseRate("2024-10-01", data.frame()),
               "'calib.date' should be of class 'Date' or 'character'")
  expect_error(calc_SourceDoseRate("2018-01-02", "2014-12-19", 0.0438, 0.0019,
                                   source.type = "error"),
               "'source.type' should be one of 'Sr-90', 'Am-214', 'Co-60' or")
  expect_error(calc_SourceDoseRate("2018-01-02", "2014-12-19", 0.0438, 0.0019,
                                   dose.rate.unit = "error"),
               "'dose.rate.unit' should be one of 'Gy/s' or 'Gy.min'")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ##simple run
  expect_silent(calc_SourceDoseRate(
    calib.date = "2014-12-19",
    calib.dose.rate = 0.0438,
    calib.error = 0.0019
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
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  expect_snapshot_RLum(calc_SourceDoseRate(measurement.date = "2012-01-27",
                                           calib.date = "2014-12-19",
                                           calib.dose.rate = 0.0438,
                                           calib.error = 0.0019,
                                           predict = 10),
                       tolerance = snapshot.tolerance)
})
