context("calc_AverageDose")

data(ExampleData.DeValues, envir = environment())
temp <- calc_AverageDose(ExampleData.DeValues$CA1[1:56,],
                       sigma_m = 0.1,
                       plot = FALSE,
                       verbose = FALSE)

test_that("check class and length of output", {
  testthat::skip_on_cran()
  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 3)

})

test_that("check summary output", {
  testthat::skip_on_cran()
  results <- get_RLum(temp)

  expect_equal(round(results$AVERAGE_DOSE, digits = 4), 65.3597)
  expect_equal(round(results$SIGMA_D, digits = 4), 0.3092)
  expect_equal(round(results$L_MAX, digits = 5), -19.25096)
})

