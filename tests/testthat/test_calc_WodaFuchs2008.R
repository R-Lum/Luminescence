test_that("Test general functionality", {
  testthat::skip_on_cran()
  local_edition(3)

  ##load example data
  ## read example data set
  data(ExampleData.DeValues, envir = environment())

  ##test arguments
  expect_s4_class(suppressWarnings(calc_WodaFuchs2008(data = ExampleData.DeValues$CA1)), "RLum.Results")

  ##test arguments
  expect_s4_class(suppressWarnings(
    calc_WodaFuchs2008(data = ExampleData.DeValues$CA1, plot = FALSE)), "RLum.Results")

  ##test arguments
  expect_s4_class(calc_WodaFuchs2008(data = ExampleData.DeValues$CA1, breaks = 20), "RLum.Results")

})
