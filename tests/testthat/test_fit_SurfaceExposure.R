data("ExampleData.SurfaceExposure", envir = environment())
d1 <- ExampleData.SurfaceExposure$sample_1
d2 <- ExampleData.SurfaceExposure$sample_2
d3 <- ExampleData.SurfaceExposure$set_1
d4 <- ExampleData.SurfaceExposure$set_2

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(fit_SurfaceExposure("test"),
               "'data' should be of class 'data.frame'")
  expect_error(fit_SurfaceExposure(list()),
               "'data' cannot be an empty list")
  expect_error(fit_SurfaceExposure(data.frame()),
               "'data' cannot be an empty data.frame")
  expect_error(fit_SurfaceExposure(matrix()),
               "'data' should have at least two columns")
  expect_error(fit_SurfaceExposure(list(d1)),
               "'age' must be of the same length")
  expect_error(fit_SurfaceExposure(d4, age = 1e4),
               "'age' must be of the same length")

  SW({
  expect_message(fit_SurfaceExposure(rbind(d1, NA), mu = 0.9),
                 "\\[fit\\_SurfaceExposure\\(\\)\\] NA values in 'data' were removed")
  })
})


test_that("check values from output example", {
  testthat::skip_on_cran()

  fit <- fit_SurfaceExposure(data = d1, sigmaphi = 5e-10, mu = 0.9,
                             plot = TRUE, verbose = FALSE)

  ## Example data 1
  expect_s4_class(
    fit_SurfaceExposure(data = d1, sigmaphi = 5e-10, mu = 0.9, age = 12000,
    plot = FALSE, verbose = FALSE), "RLum.Results")

  expect_equal(is(fit), c("RLum.Results", "RLum"))
  expect_equal(length(fit), 5)
  expect_equal(is(fit$fit), "nls")

  expect_equal(round(fit$summary$age), 9893)
  expect_equal(round(fit$summary$age_error), 369)
})

# Sub-test - weighted fitting
fit <- fit_SurfaceExposure(data = d1, sigmaphi = 5e-10, mu = 0.9, weights = TRUE,
                           plot = FALSE, verbose = FALSE)

test_that("check values from output example", {
  testthat::skip_on_cran()

  expect_equal(round(fit$summary$age), 9624)
  expect_equal(round(fit$summary$age_error), 273)
})


## Example data 2
fit <- fit_SurfaceExposure(data = data.table(d2), age = 1e4,
                           sigmaphi = 5e-10, Ddot = 2.5, D0 = 40,
                           plot = FALSE, verbose = FALSE)

test_that("check values from output example", {
  testthat::skip_on_cran()

  expect_equal(is(fit), c("RLum.Results", "RLum"))
  expect_equal(length(fit), 5)
  expect_equal(is(fit$fit), "nls")

  expect_equal(round(fit$summary$mu, 3), 0.904)
  expect_equal(round(fit$summary$mu_error, 3), 0.007)
})


## Example data 3
fit <- fit_SurfaceExposure(data = d3, age = c(1e3, 1e4, 1e5, 1e6), sigmaphi = 5e-10,
                           plot = FALSE, verbose = FALSE)

test_that("check values from output example", {
  testthat::skip_on_cran()

  expect_equal(is(fit), c("RLum.Results", "RLum"))
  expect_equal(nrow(fit$summary), 4)
  expect_equal(length(fit), 5)
  expect_equal(is(fit$fit), "nls")

  expect_equal(round(unique(fit$summary$mu), 3), 0.901)
  expect_equal(round(unique(fit$summary$mu_error), 3), 0.002)
})


## Example data 4
fit <- fit_SurfaceExposure(data = d4, age = c(1e2, 1e3, 1e4, 1e5, 1e6), sigmaphi = 5e-10,
                           Ddot = 1.0, D0 = 40,
                           plot = FALSE, verbose = FALSE)

test_that("check values from output example", {
  testthat::skip_on_cran()
  expect_equal(is(fit), c("RLum.Results", "RLum"))
  expect_equal(nrow(fit$summary), 5)
  expect_equal(length(fit), 5)
  expect_equal(is(fit$fit), "nls")

  expect_equal(round(unique(fit$summary$mu), 3), 0.899)
  expect_equal(round(unique(fit$summary$mu_error), 3), 0.002)
})

#### WARNINGS & FAILURES
test_that("not enough parameters provided", {
  testthat::skip_on_cran()

  SW({
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
                        plot = TRUE, coord_flip = TRUE, verbose = TRUE),
    "is not supported when"
  )

  expect_message(res <- fit_SurfaceExposure(list(d1, d1), age = c(NA, 1e4),
                                            sigmaphi = NA, mu = NA,
                                            log = "y", plot = TRUE),
                 "Original error from minpack.lm::nlsLM(): evaluation of fn",
                 fixed = TRUE)
  expect_message(fit_SurfaceExposure(res),
                 "Original error from minpack.lm::nlsLM(): singular gradient",
                 fixed = TRUE)
  expect_message(fit_SurfaceExposure(as.matrix(d1), log = "y"),
                 "Original error from minpack.lm::nlsLM(): singular gradient",
                 fixed = TRUE)
  })
})
