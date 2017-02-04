context("analyse_IRSAR.RF")

test_that("check class and length of output", {

  set.seed(1)
  data(ExampleData.RLum.Analysis, envir = environment())
  results_fit <- analyse_IRSAR.RF(object = IRSAR.RF.Data, plot = FALSE, method = "FIT")
  results_slide <- analyse_IRSAR.RF(object = IRSAR.RF.Data, plot = FALSE, method = "SLIDE", n.MC = NULL)
  results_slide_alt <-
    analyse_IRSAR.RF(
      object = IRSAR.RF.Data,
      plot = FALSE,
      method = "SLIDE",
      n.MC = 10,
      method.control = list(vslide_range = 'auto'),
      txtProgressBar = FALSE
    )


  expect_equal(is(results_fit), c("RLum.Results", "RLum"))
  expect_equal(length(results_fit), 5)
  expect_equal(length(results_slide), 5)
  expect_is(results_fit$fit, class = "nls", info = NULL, label = NULL)
  expect_is(results_slide$fit, class = "nls", info = NULL, label = NULL)
  expect_length(results_slide$slide, 10)


  expect_equal(results_fit$data$DE, 623.25)
  expect_equal(results_fit$data$DE.LOWER, 600.63)
  expect_equal(results_slide$data$DE, 610.17)
  expect_equal(round(results_slide_alt$data$DE, digits = 0), 384)


})
