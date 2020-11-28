data(ExampleData.DeValues, envir = environment())

temp <- calc_FiniteMixture(
  ExampleData.DeValues$CA1,
  sigmab = 0.2,
  n.components = 2,
  grain.probability = TRUE,
  verbose = FALSE)


test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_s4_class(temp, "RLum.Results")
  expect_equal(length(temp), 10)

})

test_that("check values from output example 1", {
  testthat::skip_on_cran()
  local_edition(3)

  results <- get_RLum(temp)

  expect_equal(results$de[1], 31.5299)
  expect_equal(results$de[2], 72.0333)
  expect_equal(results$de_err[1], 3.6387)
  expect_equal(results$de_err[2], 2.4082)
  expect_equal(results$proportion[1], 0.1096)
  expect_equal(results$proportion[2], 0.8904)

})
