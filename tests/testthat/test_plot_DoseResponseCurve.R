## load data
data(ExampleData.LxTxData, envir = environment())
fit <- fit_DoseResponseCurve(LxTxData, verbose = FALSE)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(
      plot_DoseResponseCurve("error"),
      "[plot_DoseResponseCurve()] 'object' should be of class 'RLum.Results'",
      fixed = TRUE)
  expect_error(
      plot_DoseResponseCurve(fit, plot_extended = "error"),
      "'plot_extended' should be of class 'logical'")
  expect_error(
      plot_DoseResponseCurve(fit, plot_single = "error"),
      "'plot_single' should be of class 'logical'")
  expect_error(
      plot_DoseResponseCurve(fit, verbose = "error"),
      "'verbose' should be of class 'logical'")
  expect_error(
      plot_DoseResponseCurve(fit, cex.global = 0),
      "'cex.global' should be a positive scalar")
})
