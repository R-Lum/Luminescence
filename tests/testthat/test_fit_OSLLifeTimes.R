context("fit_OSLLifeTimes()")

##load example data
data(ExampleData.TR_OSL, envir = environment())


test_that("standard check", {
  testthat::skip_on_cran()

  ##trgger errors
  expect_null(fit_OSLLifeTimes(object = "test"))

  ##simple run
  expect_s4_class(object = fit_OSLLifeTimes(
    object = ExampleData.TR_OSL,
    n.components = 1), class = "RLum.Results")

  ##test options
  expect_s4_class(object = fit_OSLLifeTimes(
    object = ExampleData.TR_OSL,
    verbose = FALSE,
    plot = FALSE,
    n.components = 1), class = "RLum.Results")


})

