context("analyse_IRSAR.RF")

data(ExampleData.RLum.Analysis, envir = environment())
results_fit <- analyse_IRSAR.RF(object = IRSAR.RF.Data, plot = FALSE, method = "FIT")
results_slide <- analyse_IRSAR.RF(object = IRSAR.RF.Data, plot = FALSE, method = "SLIDE", n.MC = NULL)

test_that("check class and length of output", {

  expect_equal(is(results_fit), c("RLum.Results", "RLum"))
  expect_equal(length(results_fit), 5)
  expect_equal(length(results_slide), 5)
  expect_is(results_fit$fit, class = "nls", info = NULL, label = NULL)
  expect_is(results_slide$fit, class = "nls", info = NULL, label = NULL)
  expect_length(results_slide$slide, 10)

})

test_that("check values from output example", {

  expect_equal(results_fit$data$DE, 623.25)
  expect_equal(results_fit$data$DE.LOWER, 600.63)
  expect_equal(results_slide$data$DE, 610.17)

})
