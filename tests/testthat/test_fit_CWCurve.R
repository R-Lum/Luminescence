## load data
data(ExampleData.CW_OSL_Curve, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(fit_CWCurve("error"),
               "'values' should be of class 'RLum.Data.Curve' or 'data.frame'")
  expect_error(fit_CWCurve(data.frame()),
               "'values' cannot be an empty data.frame")
  expect_error(fit_CWCurve(set_RLum("RLum.Data.Curve")),
               "'values' contains no positive counts")
  expect_error(fit_CWCurve(ExampleData.CW_OSL_Curve, fit.method = "error"),
               "'fit.method' should be one of 'port' or 'LM'")
  expect_error(fit_CWCurve(ExampleData.CW_OSL_Curve, n.components.max = 0),
               "'n.components.max' should be a positive integer scalar")
  expect_error(fit_CWCurve(ExampleData.CW_OSL_Curve, fit.failure_threshold = -1),
               "'fit.failure_threshold' should be a positive integer scalar")
  expect_error(fit_CWCurve(ExampleData.CW_OSL_Curve[5:1, ]),
               "Time values are not ordered")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## data.frame
  SW({
  fit <- fit_CWCurve(values = ExampleData.CW_OSL_Curve,
                     main = "CW Curve Fit",
                     n.components.max = 4,
                     log = "x",
                     plot = FALSE)
  })
  expect_s4_class(fit, "RLum.Results")
  expect_equal(length(fit), 3)
  expect_equal(fit$data$n.components, 3, tolerance = 1)
  expect_equal(round(fit$data$I01, digits = 0), 2388, tolerance = 1)
  expect_equal(round(fit$data$lambda1, digits = 1), 4.6, tolerance = 1)
  expect_equal(round(fit$data$`pseudo-R^2`, digits = 0), 1)

  ## RLum.Data.Curve object
  curve <- set_RLum("RLum.Data.Curve",
                    data = as.matrix(ExampleData.CW_OSL_Curve),
                    curveType = "measured",
                    recordType = "OSL")

  SW({
  fit <- fit_CWCurve(values = curve,
                     main = "CW Curve Fit",
                     n.components.max = 4,
                     log = "x",
                     plot = FALSE)
  })
  expect_s4_class(fit, "RLum.Results")
  expect_equal(length(fit), 3)
  expect_equal(fit$data$n.components, 3, tolerance = 1)
  expect_equal(round(fit$data$I01, digits = 0), 2388, tolerance = 1)
  expect_equal(round(fit$data$lambda1, digits = 1), 4.6, tolerance = 1)
  expect_equal(round(fit$data$`pseudo-R^2`, digits = 0), 1)

  SW({
  expect_warning(fit_CWCurve(ExampleData.CW_OSL_Curve, fit.method = "LM",
                             fit.calcError = TRUE, xlab = "x", ylab = "y",
                             log = "x", output.path = tempdir()),
                 "Argument 'output.path' no longer supported")
  })

  ## more coverage
  expect_message(fit_CWCurve(ExampleData.CW_OSL_Curve[1, ]),
                 "Error: Fitting failed, plot without fit produced")
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 509
  SW({
  expect_message(fit_CWCurve(ExampleData.CW_OSL_Curve[1:20, ],
                             fit.method = "LM", fit.calcError = TRUE),
                 "Error: Computation of confidence interval failed")
  })
})
