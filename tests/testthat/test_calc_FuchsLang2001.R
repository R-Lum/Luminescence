data(ExampleData.DeValues, envir = environment())
temp <- calc_FuchsLang2001(ExampleData.DeValues$BT998,
                           cvThreshold = 5,
                           plot = FALSE,
                           verbose = FALSE)


test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_s4_class(temp, "RLum.Results")
  expect_equal(length(temp), 4)

})

test_that("check values from output example 1", {
  testthat::skip_on_cran()
  local_edition(3)

  results <- get_RLum(temp)

  expect_equal(results$de, 2866.11)
  expect_equal(results$de_err, 157.35)
  expect_equal(results$de_weighted, 2846.66)
  expect_equal(results$de_weighted_err, 20.58)
  expect_equal(results$n.usedDeValues, 22)

})
