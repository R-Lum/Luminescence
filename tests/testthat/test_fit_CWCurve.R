data(ExampleData.CW_OSL_Curve, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(fit_CWCurve("error"),
               "Input object is not of type 'RLum.Data.Curve' or 'data.frame'")
  expect_error(fit_CWCurve(ExampleData.CW_OSL_Curve, fit.method = "error"),
               "'fit.method' unknown")
})

test_that("check class and length of output", {
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
                             output.path = tempdir()),
                 "Argument 'output.path' no longer supported")
  })
})
