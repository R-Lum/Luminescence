data("ExampleData.SurfaceExposure", envir = environment())
d1 <- ExampleData.SurfaceExposure$sample_1
d2 <- ExampleData.SurfaceExposure$sample_2
d3 <- ExampleData.SurfaceExposure$set_1
d4 <- ExampleData.SurfaceExposure$set_2

## Example data 1
fit <- fit_SurfaceExposure(data = d1, sigmaphi = 5e-10, mu = 0.9,
                           plot = FALSE, verbose = FALSE)

test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_equal(is(fit), c("RLum.Results", "RLum"))
  expect_equal(length(fit), 5)
  expect_equal(is(fit$fit), "nls")
})

test_that("check values from output example", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_equal(round(fit$summary$age), 9893)
  expect_equal(round(fit$summary$age_error), 369)
})

# Sub-test - weighted fitting
fit <- fit_SurfaceExposure(data = d1, sigmaphi = 5e-10, mu = 0.9, weights = TRUE,
                           plot = FALSE, verbose = FALSE)


test_that("check values from output example", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_equal(round(fit$summary$age), 9624)
  expect_equal(round(fit$summary$age_error), 273)
})


## Example data 2
fit <- fit_SurfaceExposure(data = d2, age = 1e4, sigmaphi = 5e-10, Ddot = 2.5, D0 = 40,
                           plot = FALSE, verbose = FALSE)

test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_equal(is(fit), c("RLum.Results", "RLum"))
  expect_equal(length(fit), 5)
  expect_equal(is(fit$fit), "nls")
})

test_that("check values from output example", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_equal(round(fit$summary$mu, 3), 0.904)
  expect_equal(round(fit$summary$mu_error, 3), 0.007)
})


## Example data 3
fit <- fit_SurfaceExposure(data = d3, age = c(1e3, 1e4, 1e5, 1e6), sigmaphi = 5e-10,
                           plot = FALSE, verbose = FALSE)

test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_equal(is(fit), c("RLum.Results", "RLum"))
  expect_equal(nrow(fit$summary), 4)
  expect_equal(length(fit), 5)
  expect_equal(is(fit$fit), "nls")
})

test_that("check values from output example", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_equal(round(unique(fit$summary$mu), 3), 0.901)
  expect_equal(round(unique(fit$summary$mu_error), 3), 0.002)
})


## Example data 4
fit <- fit_SurfaceExposure(data = d4, age = c(1e2, 1e3, 1e4, 1e5, 1e6), sigmaphi = 5e-10,
                           Ddot = 1.0, D0 = 40,
                           plot = FALSE, verbose = FALSE)

test_that("check class and length of output", {
  testthat::skip_on_cran()
  expect_equal(is(fit), c("RLum.Results", "RLum"))
  expect_equal(nrow(fit$summary), 5)
  expect_equal(length(fit), 5)
  expect_equal(is(fit$fit), "nls")
})

test_that("check values from output example", {
  testthat::skip_on_cran()
  expect_equal(round(unique(fit$summary$mu), 3), 0.899)
  expect_equal(round(unique(fit$summary$mu_error), 3), 0.002)
})

#### WARNINGS & FAILURES
test_that("not enough parameters provided", {
  testthat::skip_on_cran()
  expect_message(
    fit_SurfaceExposure(data = d1, plot = FALSE, verbose = TRUE),
    "Unable to fit the data"
  )
  expect_equal(
    is(fit_SurfaceExposure(data = d2, plot = FALSE, verbose = FALSE)$fit),
    "simpleError"
  )
  expect_warning(
    fit_SurfaceExposure(data = d4, age = c(1e2, 1e3, 1e4, 1e5, 1e6), sigmaphi = 5e-10,
                        Ddot = 1.0, D0 = 40, weights = TRUE,
                        plot = FALSE, verbose = TRUE),
    "is not supported when"
  )
  expect_error(
    fit_SurfaceExposure(data = d4, age = 1e4, sigmaphi = 5e-10,
                        Ddot = 1.0, D0 = 40,
                        plot = FALSE, verbose = FALSE),
    "'age' must be of the same length"
  )

})

