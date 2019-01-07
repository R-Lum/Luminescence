context("fit_SurfaceExposure")

data("ExampleData.ScaleGammaDose", envir = environment())
d <- ExampleData.ScaleGammaDose

## Conversion factors: Liritzisetal2013
results <- scale_GammaDose(data = d, 
                           conversion_factors = "Liritzisetal2013", 
                           fractional_gamma_dose = "Aitken1985",
                           plot = FALSE, verbose = FALSE)

test_that("check class and length of output", {
  testthat::skip_on_cran()
  expect_equal(is(results), c("RLum.Results", "RLum"))
  expect_equal(length(results), 6)
  expect_equal(is(results$summary)[1], "data.frame")
})

test_that("check values from output example", {
  testthat::skip_on_cran()
  expect_equal(formatC(results$summary$dose_rate_total, 4), "0.9242")
  expect_equal(formatC(results$summary$dose_rate_total_err, 4), "0.2131")
})

## Conversion factors: Guerinetal2011
results <- scale_GammaDose(data = d, 
                           conversion_factors = "Guerinetal2011", 
                           fractional_gamma_dose = "Aitken1985",
                           plot = FALSE, verbose = FALSE)

test_that("check class and length of output", {
  testthat::skip_on_cran()
  expect_equal(is(results), c("RLum.Results", "RLum"))
  expect_equal(length(results), 6)
  expect_equal(is(results$summary)[1], "data.frame")
})

test_that("check values from output example", {
  testthat::skip_on_cran()
  expect_equal(formatC(results$summary$dose_rate_total, 4), "0.9214")
  expect_equal(formatC(results$summary$dose_rate_total_err, 4), "0.2124")
})

## Conversion factors: Guerinetal2011
results <- scale_GammaDose(data = d, 
                           conversion_factors = "AdamiecAitken1998", 
                           fractional_gamma_dose = "Aitken1985",
                           plot = FALSE, verbose = FALSE)

test_that("check class and length of output", {
  testthat::skip_on_cran()
  expect_equal(is(results), c("RLum.Results", "RLum"))
  expect_equal(length(results), 6)
  expect_equal(is(results$summary)[1], "data.frame")
})

test_that("check values from output example", {
  testthat::skip_on_cran()
  expect_equal(formatC(results$summary$dose_rate_total, 4), "0.9123")
  expect_equal(formatC(results$summary$dose_rate_total_err, 4), "0.2097")
})

## CONSOLE & PLOT OUTPUT
test_that("console & plot", {
  expect_output({
   scale_GammaDose(d, plot = TRUE, verbose = TRUE) 
   scale_GammaDose(d, plot = TRUE, plot_single = FALSE, verbose = TRUE) 
  })
})


## WARNINGS & FAILURES
test_that("check input data", {
  testthat::skip_on_cran()
  expect_error(
    scale_GammaDose(NA, plot = FALSE, verbose = TRUE),
    "must be a data frame"
  )
  expect_error(
    scale_GammaDose(d[ ,1:10], plot = FALSE, verbose = TRUE),
    "must have 12 columns"
  )
  expect_warning({
    tmp <- d
    colnames(tmp) <- letters[1:ncol(tmp)]
    scale_GammaDose(tmp, plot = FALSE, verbose = TRUE)
    },
    "Unexpected column names"
  )
  expect_error({
    tmp <- d
    tmp$sample_offset <- NA
    scale_GammaDose(tmp, plot = FALSE, verbose = TRUE)
    },
    "Only one layer must be contain a numeric value in column 'sample_offset'"
  )
  expect_error({
    tmp <- d
    tmp$sample_offset[5] <- "target"
    scale_GammaDose(tmp, plot = FALSE, verbose = TRUE)
    },
    "Non-numeric value in the the row of the target layer."
  )
  expect_error({
    tmp <- d
    tmp$sample_offset[5] <- -1
    scale_GammaDose(tmp, plot = FALSE, verbose = TRUE)
  },
  "The numeric value in 'sample_offset' must be positive."
  )
  expect_error({
    tmp <- d
    tmp$sample_offset[5] <- 20
    scale_GammaDose(tmp, plot = FALSE, verbose = TRUE)
  },
  "Impossible! Sample offset larger than the target-layer's thickness!"
  )
  expect_error({
    scale_GammaDose(d, conversion_factors = c("a", "b"), plot = FALSE, verbose = TRUE)
  },
  "must be an object of length 1 and of class 'character'."
  )
  expect_error({
    scale_GammaDose(d, conversion_factors = 1, plot = FALSE, verbose = TRUE)
  },
  "must be an object of length 1 and of class 'character'."
  )
  expect_error({
    scale_GammaDose(d, conversion_factors = "HansGuenter2020", plot = FALSE, verbose = TRUE)
  },
  "Invalid 'conversion_factors'. Valid options:"
  )
  expect_error({
    scale_GammaDose(d, fractional_gamma_dose = c("a", "b"), plot = FALSE, verbose = TRUE)
  },
  "must be an object of length 1 and of class 'character'."
  )
  expect_error({
    scale_GammaDose(d, fractional_gamma_dose = 1, plot = FALSE, verbose = TRUE)
  },
  "must be an object of length 1 and of class 'character'."
  )
  expect_error({
    scale_GammaDose(d, fractional_gamma_dose = "momgetthecameraiamontheinternet1995", plot = FALSE, verbose = TRUE)
  },
  "Invalid 'fractional_gamma_dose'. Valid options:"
  )
})

