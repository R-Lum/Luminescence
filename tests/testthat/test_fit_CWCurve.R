## load data
data(ExampleData.CW_OSL_Curve, envir = environment())
data(ExampleData.FittingLM, envir = environment())
curve <- set_RLum("RLum.Data.Curve",
                  data = as.matrix(ExampleData.CW_OSL_Curve),
                  curveType = "measured",
                  recordType = "OSL")
curve@data <- curve@data[1:90, ]

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(fit_CWCurve("error"),
               "'object' should be of class 'RLum.Data.Curve' or 'data.frame'")
  expect_error(fit_CWCurve(data.frame()),
               "'object' cannot be an empty data.frame")
  expect_error(fit_CWCurve(iris[, 1, drop = FALSE]),
               "'object' should have 2 columns")
  expect_error(fit_CWCurve(data.frame(a = 1:10, b = NA)),
               "'object' contains no positive counts")
  expect_error(fit_CWCurve(set_RLum("RLum.Data.Curve")),
               "'object' contains no positive counts")
  expect_error(fit_CWCurve(ExampleData.CW_OSL_Curve, fit.method = "error"),
               "'fit.method' should be one of 'port' or 'LM'")
  expect_error(fit_CWCurve(ExampleData.CW_OSL_Curve, n.components.max = 0),
               "'n.components.max' should be a single positive integer value")
  expect_error(fit_CWCurve(ExampleData.CW_OSL_Curve, fit.failure_threshold = -1),
               "'fit.failure_threshold' should be a single positive integer value")
  expect_error(fit_CWCurve(ExampleData.CW_OSL_Curve, verbose = "error"),
               "'verbose' should be a single logical value")
  expect_error(fit_CWCurve(ExampleData.CW_OSL_Curve, output.terminalAdvanced = "error"),
               "'output.terminalAdvanced' should be a single logical value")
  expect_error(fit_CWCurve(ExampleData.CW_OSL_Curve, plot = "error"),
               "'plot' should be a single logical value")
  expect_error(fit_CWCurve(ExampleData.CW_OSL_Curve[5:1, ]),
               "Time values are not ordered")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  SW({
  ## data.frame
  expect_snapshot_RLum(fit_CWCurve(ExampleData.CW_OSL_Curve,
                                   n.components.max = 3,
                                   fit.method = "LM",
                                   plot = FALSE),
                       tolerance = snapshot.tolerance)

  ## RLum.Data.Curve object
  expect_snapshot_RLum(fit_CWCurve(curve,
                                   n.components.max = 2,
                                   fit.calcError = TRUE,
                                   method_control = list(export.comp.contrib.matrix = TRUE),
                                   verbose = FALSE,
                                   plot = FALSE),
                       tolerance = snapshot.tolerance)
  })
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  ## FIXME(mcol): n.components.max is set to 2 due to failures in CI that
  ## are not reproducible on Ubuntu 22.04 with R 4.5.1 or R-devel: the Ubuntu
  ## 24.04 with R-devel on CI tend to pick more than 2 components, while
  ## locally only 2 are chosen even when the maximum is higher
  SW({
  vdiffr::expect_doppelganger("default",
                              fit_CWCurve(ExampleData.CW_OSL_Curve,
                                          n.components.max = 2))
  vdiffr::expect_doppelganger("logx cex",
                              fit_CWCurve(ExampleData.CW_OSL_Curve,
                                          main = "CW Curve Fit",
                                          n.components.max = 2,
                                          cex.global = 2,
                                          log = "x"))
  })
})

test_that("more coverage", {
  testthat::skip_on_cran()

  expect_message(fit_CWCurve(ExampleData.CW_OSL_Curve[1, ]),
                 "Error: Fitting failed, plot without fit produced")

  pdf(tempfile(), width = 1, height = 1)
  expect_message(fit_CWCurve(values.curve, n.components.max = 3,
                             verbose = FALSE),
                 "Figure margins too large or plot area too small")
  pdf(tempfile(), width = 1, height = 1)
  expect_message(fit_CWCurve(ExampleData.CW_OSL_Curve, verbose = FALSE),
                 "Figure margins too large or plot area too small")
  expect_error(fit_CWCurve(data.frame(NA, 1:5)),
               "0 (non-NA) cases", fixed = TRUE)

  ## deprecated argument
  SW({
  expect_warning(fit_CWCurve(values = ExampleData.CW_OSL_Curve,
                             n.components.max = 2),
                 "'values' was deprecated in v1.2.0, use 'object' instead")
  })
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 509
  SW({
  expect_message(fit_CWCurve(ExampleData.CW_OSL_Curve[1:20, ],
                             fit.method = "LM", fit.calcError = TRUE),
                 "Error: Computation of confidence interval failed")
  })

  ## issue 953
  SW({
  fit_CWCurve(ExampleData.CW_OSL_Curve[1:2, ], fit.trace = TRUE)
  })
})
