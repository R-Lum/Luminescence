test_that("Test general functionality", {
  testthat::skip_on_cran()

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

  ## issue #197
  set.seed(1)
  df <- data.frame(rnorm(20, 10), rnorm(20, 0.5))
  expect_silent(calc_WodaFuchs2008(df))
})
