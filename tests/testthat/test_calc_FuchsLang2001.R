context("calc_FuchsLang2001")

data(ExampleData.DeValues, envir = environment())
temp <- calc_FuchsLang2001(ExampleData.DeValues$BT998, 
                           cvThreshold = 5, 
                           plot = FALSE, 
                           verbose = FALSE)


test_that("check class and length of output", {
  
  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 5)
  
})

test_that("check values from output example 1", {
  
  
  results <- get_RLum(temp)
  
  expect_equal(results$de, 2866.11)
  expect_equal(results$de_err, 157.35)
  expect_equal(results$de_weighted, 2846.66)
  expect_equal(results$de_weighted_err, 20.58)
  expect_equal(results$n.usedDeValues, 22)
  
})
