context("test_calc_WodaFuchs2008")

test_that("Test general functionality", {
  testthat::skip_on_cran()

  ##load example data
  ## read example data set
  data(ExampleData.DeValues, envir = environment())

  ##test arguments
  expect_is(calc_WodaFuchs2008(data = ExampleData.DeValues$CA1), "RLum.Results")

  ##test arguments
  expect_is(calc_WodaFuchs2008(data = ExampleData.DeValues$CA1, plot = FALSE), "RLum.Results")

  ##test arguments
  expect_is(calc_WodaFuchs2008(data = ExampleData.DeValues$CA1, breaks = 20), "RLum.Results")

})
