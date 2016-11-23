context("calc_HomogeneityTest")

data(ExampleData.DeValues, envir = environment())
temp <- calc_HomogeneityTest(ExampleData.DeValues$BT998,
                           verbose = FALSE)


test_that("check class and length of output", {
  
  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 4)
  
})

test_that("check values from output example", {
  
  
  results <- get_RLum(temp)
  
  expect_equal(results$n, 25)
  expect_equal(results$g.value, 0.008687915)
  expect_equal(results$df, 24)
  expect_equal(results$P.value, 1)
  
})
