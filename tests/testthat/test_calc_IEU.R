context("calc_IEU")

data(ExampleData.DeValues, envir = environment())
temp <- calc_IEU(ExampleData.DeValues$CA1,
                 a = 0.2,
                 b = 1.9,
                 interval = 1, verbose = FALSE, plot = FALSE)

test_that("check class and length of output", {
  testthat::skip_on_cran()
  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 5)

})

test_that("check values from output example", {
  testthat::skip_on_cran()

  results <- get_RLum(temp)

  expect_equal(results$de, 46.67)
  expect_equal(results$de_err, 2.55)
  expect_equal(results$n, 24)

})
