context("fit_CWCurve")

data(ExampleData.CW_OSL_Curve, envir = environment())
fit <- fit_CWCurve(values = ExampleData.CW_OSL_Curve,
                   main = "CW Curve Fit",
                   n.components.max = 4,
                   log = "x")

test_that("check class and length of output", {

  expect_equal(is(fit), c("RLum.Results", "RLum"))
  expect_equal(length(fit), 3)

})

test_that("check values from output example", {

  expect_equal(fit$data$n.components, 3)
  expect_equal(round(fit$data$I01, digits = 3), 2387.617)

})
