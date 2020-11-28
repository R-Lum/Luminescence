data(ExampleData.CW_OSL_Curve, envir = environment())
fit <- fit_CWCurve(values = ExampleData.CW_OSL_Curve,
                   main = "CW Curve Fit",
                   n.components.max = 4,
                   log = "x",
                   plot = FALSE)

test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_s4_class(fit, "RLum.Results")
  expect_equal(length(fit), 3)

})

test_that("check values from output example", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_equal(fit$data$n.components, 3)
  expect_equal(round(fit$data$I01, digits = 0), 2388)
  expect_equal(round(fit$data$lambda1, digits = 1), 4.6)
  expect_equal(round(fit$data$`pseudo-R^2`, digits = 0), 1)

})
