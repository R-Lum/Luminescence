test_that("Test various S3 methods", {
  testthat::skip_on_cran()
  local_edition(3)

  ## create test data
  data(ExampleData.CW_OSL_Curve, envir = environment())
  data(ExampleData.BINfileData, envir = environment())
  data(ExampleData.DeValues, envir = environment())
  data(ExampleData.DeValues, envir = environment())

  temp <- as(ExampleData.CW_OSL_Curve, "RLum.Data.Curve")

  dose.rate <-  calc_SourceDoseRate(
    measurement.date = "2012-01-27",
    calib.date = "2014-12-19",
    calib.dose.rate = 0.0438,
    calib.error = 0.0019)


  ## plotting ----
  expect_silent(plot(list(temp, temp)))
  expect_silent(plot(subset(CWOSL.SAR.Data, ID == 1)))
  expect_silent(hist(dose.rate))




})
