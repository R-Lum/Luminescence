context("calc_FiniteMixture")

data(ExampleData.DeValues, envir = environment())

temp <- calc_FiniteMixture(
  ExampleData.DeValues$CA1,
  sigmab = 0.2, 
  n.components = 2,
  grain.probability = TRUE, 
  verbose = FALSE)


test_that("check class and length of output", {
  
  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 10)
  
})

test_that("check values from output example 1", {
  
  results <- get_RLum(temp)
  
  expect_equal(results$de[1], 31.5299)
  expect_equal(results$de[2], 72.0333)
  expect_equal(results$de_err[1], 3.6387)
  expect_equal(results$de_err[2], 2.4082)
  expect_equal(results$proportion[1], 0.1096)
  expect_equal(results$proportion[2], 0.8904)
  
})