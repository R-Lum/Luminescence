data(ExampleData.DeValues, envir = environment())
temp <- calc_MaxDose(ExampleData.DeValues$CA1,
                     sigmab = 0.2,
                     par = 3,
                     plot = TRUE,
                     verbose = FALSE)

test_that("check class and length of output", {
  testthat::skip_on_cran()

  expect_s4_class(temp, "RLum.Results")
  expect_equal(length(temp), 9)

})

test_that("check values from output example", {
  testthat::skip_on_cran()

  results <- get_RLum(temp)

  expect_equal(round(results$de, digits = 2), 76.58)
  expect_equal(round(results$de_err, digits = 2), 7.57)
  expect_equal(results$ci_level, 0.95)
  expect_equal(round(results$ci_lower, digits = 2), 69.65)
  expect_equal(round(results$ci_upper, digits = 2), 99.33)
  expect_equal(results$par, 3)
  expect_equal(round(results$sig, digits = 2), 1.71)
  expect_equal(round(results$p0, digits = 2), 0.65)
  expect_equal(results$mu, NA)
  expect_equal(round(results$Lmax, digits = 2), -19.79)
  expect_equal(round(results$BIC, digits = 2), 58.87)

})
