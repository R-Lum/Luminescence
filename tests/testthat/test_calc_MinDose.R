context("calc_MinDose")

data(ExampleData.DeValues, envir = environment())
temp <- calc_MinDose(data = ExampleData.DeValues$CA1,
                     sigmab = 0.1,
                     verbose = FALSE,
                     plot = FALSE)

test_that("check class and length of output", {
  testthat::skip_on_cran()
  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 9)

})

test_that("check values from output example", {
  testthat::skip_on_cran()

  results <- get_RLum(temp)

  expect_equal(round(results$de, digits = 5), 34.31834)
  expect_equal(round(results$de_err, digits = 6), 2.550964)
  expect_equal(results$ci_level, 0.95)
  expect_equal(round(results$ci_lower, digits = 5), 29.37526)
  expect_equal(round(results$ci_upper, digits = 5), 39.37503)
  expect_equal(results$par, 3)
  expect_equal(round(results$sig, digits = 7), 0.7287325)
  expect_equal(round(results$p0, digits = 8), 0.01053938)
  expect_equal(results$mu, NA)
  expect_equal(round(results$Lmax, digits = 5), -43.57969)
  expect_equal(round(results$BIC, digits = 4), 106.4405)

})
