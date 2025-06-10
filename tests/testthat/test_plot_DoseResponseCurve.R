## load data
data(ExampleData.LxTxData, envir = environment())
set.seed(1)
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
})

test_that("plot output", {
  testthat::skip_on_cran()

  ## standard return
  expect_s4_class(plot_DoseResponseCurve(fit), "RLum.Results")

  ## check plot settings
  expect_s4_class(plot_DoseResponseCurve(fit, legend = FALSE,
                                         reg_points_pch = 1,
                                         density_polygon = FALSE,
                                         box = FALSE), "RLum.Results")

  ## De is NA
  df <- data.frame(DOSE = c(0,5,10,20,30), LxTx = c(10,5,-20,-30,-40), LxTx_X = c(1,1,1,1,1))
  expect_s4_class(plot_DoseResponseCurve(fit_DoseResponseCurve(df, verbose = FALSE)),
                  "RLum.Results")
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(getRversion() >= "4.4.0")

  SW({
  vdiffr::expect_doppelganger("default",
                              plot_DoseResponseCurve(fit))
  vdiffr::expect_doppelganger("extrapolation-gok",
                              plot_DoseResponseCurve(
                                  fit_DoseResponseCurve(LxTxData,
                                                        fit.method = "GOK",
                                                        mode = "extrapolation",
                                                        verbose = FALSE)))
  vdiffr::expect_doppelganger("cex.global",
                              plot_DoseResponseCurve(fit, legend = FALSE,
                                                     reg_points_pch = 1,
                                                     density_polygon_col = "azure",
                                                     cex = 2))
  })
})
