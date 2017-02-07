context("fit_LWCurve")

## Test 1 with NLS
data(ExampleData.FittingLM, envir = environment())
fit <- fit_LMCurve(values = values.curve,
            values.bg = values.curveBG,
            n.components = 3,
            log = "x",
            start_values = data.frame(Im = c(170,25,400), xm = c(56,200,1500)),
            plot = FALSE)

test_that("check class and length of output", {

  expect_equal(is(fit), c("RLum.Results", "RLum"))
  expect_equal(length(fit), 3)

})

test_that("check values from output example", {
  testthat::skip_on_cran()

  expect_equal(fit$data$n.components, 3)
  expect_equal(round(fit$data$Im1, digits = 3), 169.44)
  expect_equal(round(fit$data$xm1, digits = 5), 49.00643)
  expect_equal(round(fit$data$b1, digits = 5), 1.66554)
  expect_equal(round(fit$data$`pseudo-R^2`, digits = 4), 0.9437)

})

## Test 2 with LM
data(ExampleData.FittingLM, envir = environment())
fit <- fit_LMCurve(values = values.curve,
                   values.bg = values.curveBG,
                   n.components = 3,
                   log = "x",
                   fit.method = "LM",
                   plot = FALSE)

test_that("check class and length of output", {

  expect_equal(is(fit), c("RLum.Results", "RLum"))
  expect_equal(length(fit), 3)

})

test_that("check values from output example", {
  testthat::skip_on_cran()

  expect_equal(fit$data$n.components, 3)
  expect_equal(round(fit$data$Im1, digits = 3), 169.437)
  expect_equal(round(fit$data$xm1, digits = 5), 49.00509)
  expect_equal(round(fit$data$b1, digits = 5), 1.66563)
  expect_equal(round(fit$data$`pseudo-R^2`, digits = 4), 0.9437)

})
