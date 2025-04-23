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
      "'plot_extended' should be a single logical value")
  expect_error(
      plot_DoseResponseCurve(fit, plot_singlePanels = "error"),
      "'plot_singlePanels' should be a single logical value")
  expect_error(
      plot_DoseResponseCurve(fit, verbose = "error"),
      "'verbose' should be a single logical value")
  expect_error(
      plot_DoseResponseCurve(fit, cex.global = 0),
      "'cex.global' should be a positive scalar")
})

test_that("plot output", {
  testthat::skip_on_cran()

  ## standard return
  expect_s4_class(plot_DoseResponseCurve(fit), "RLum.Results")

  ## check plot settings
  expect_s4_class(plot_DoseResponseCurve(fit, legend = FALSE), "RLum.Results")
  expect_s4_class(plot_DoseResponseCurve(fit, reg_points_pch = 1), "RLum.Results")
  expect_s4_class(plot_DoseResponseCurve(fit, density_polygon = FALSE), "RLum.Results")
  expect_s4_class(plot_DoseResponseCurve(fit, density_rug = FALSE), "RLum.Results")
  expect_s4_class(plot_DoseResponseCurve(fit, density_polygon_col = "green"), "RLum.Results")
  expect_s4_class(plot_DoseResponseCurve(fit, box = FALSE), "RLum.Results")

  ## De is NA
  df <- data.frame(DOSE = c(0,5,10,20,30), LxTx = c(10,5,-20,-30,-40), LxTx_X = c(1,1,1,1,1))
  expect_s4_class(plot_DoseResponseCurve(fit_DoseResponseCurve(df, verbose = FALSE)),
                  "RLum.Results")
})
