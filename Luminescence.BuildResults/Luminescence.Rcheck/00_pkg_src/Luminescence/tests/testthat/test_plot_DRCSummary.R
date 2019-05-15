context("plot_DRCSummary")

test_that("Test certain input scenarios", {
  testthat::skip_on_cran()

  ##function stop
  expect_error(plot_DRCSummary("test"), regexp = "The input is not of class 'RLum.Results'")
  expect_error(plot_DRCSummary(set_RLum("RLum.Results")),
               regexp = "'object' was created by none supported function, cf. manual for allowed originators")

})

test_that("Test plotting", {
  testthat::skip_on_cran()

  #load data example data
  data(ExampleData.BINfileData, envir = environment())

  #transform the values from the first position in a RLum.Analysis object
  object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

  results <- analyse_SAR.CWOSL(
    object = object,
    signal.integral.min = 1,
    signal.integral.max = 2,
    background.integral.min = 900,
    background.integral.max = 1000,
    plot = FALSE,
    verbose = FALSE
  )

  ##simple
  expect_silent(plot_DRCSummary(results))

  ##plus points
  expect_silent(plot_DRCSummary(results, show_dose_points = TRUE, show_natural = TRUE))

  ##excpect warning
  expect_warning(plot_DRCSummary(results, show_dose_points = TRUE, show_natural = TRUE, sel_curves = 1000))

})
