context("fit_OSLLifeTimes()")

##load example data
data(ExampleData.TR_OSL, envir = environment())

temp_list <- list(ExampleData.TR_OSL, ExampleData.TR_OSL)
temp_analysis <- set_RLum("RLum.Analysis", records = temp_list)


test_that("standard check", {
  testthat::skip_on_cran()

  ##trigger errors
  expect_null(fit_OSLLifeTimes(object = "test"))

  ## Test different inputs
  ##simple run
  set.seed(1)
  expect_s4_class(object = fit_OSLLifeTimes(
    object = ExampleData.TR_OSL,
    plot = FALSE,
    n.components = 1), class = "RLum.Results")

  ##simple list
  expect_s4_class(object = fit_OSLLifeTimes(
    object = temp_list,
    plot = FALSE,
    n.components = 1), class = "RLum.Results")

  ##simple RLum.Analysis
  expect_s4_class(object = fit_OSLLifeTimes(
    object = temp_analysis,
    verbose = FALSE,
    plot = FALSE,
    n.components = 1), class = "RLum.Results")

  ##test arguments
  ##simple run
  expect_s4_class(object = fit_OSLLifeTimes(
    object = ExampleData.TR_OSL,
    method_control = list(seed = 1, weights = FALSE),
    plot = FALSE,
    n.components = 1), class = "RLum.Results")

  ##test options
  expect_s4_class(object = fit_OSLLifeTimes(
    object = ExampleData.TR_OSL,
    verbose = FALSE,
    plot = FALSE,
    n.components = 1), class = "RLum.Results")

  ##warning for log
  expect_warning(object = fit_OSLLifeTimes(
    object = ExampleData.TR_OSL,
    verbose = FALSE,
    plot = TRUE,
    log = "x",
    n.components = 1), regexp = "log-scale requires x-values > 0, set min xlim to 0.01!")

})

