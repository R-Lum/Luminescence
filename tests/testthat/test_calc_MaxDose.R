context("calc_MaxDose")

data(ExampleData.DeValues, envir = environment())
temp <- calc_MaxDose(ExampleData.DeValues$CA1, 
                     sigmab = 0.2, 
                     par = 3, 
                     plot = FALSE, 
                     verbose = FALSE)

test_that("check class and length of output", {
  
  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 9)
  
})

test_that("check values from output example", {
  
  
  results <- get_RLum(temp)
  
  expect_equal(round(results$de, digits = 5), 76.57571)
  expect_equal(round(results$de_err, digits = 6), 7.569908)
  expect_equal(results$ci_level, 0.95)
  expect_equal(round(results$ci_lower, digits = 5), 69.65358)
  expect_equal(round(results$ci_upper, digits = 5), 99.32762)
  expect_equal(results$par, 3)
  expect_equal(round(results$sig, digits = 7), 0.5376628)
  expect_equal(round(results$p0, digits = 7), 0.6482137)
  expect_equal(results$mu, NA)
  expect_equal(round(results$Lmax, digits = 5), -19.79245)
  expect_equal(round(results$BIC, digits = 5), 58.86603)
  
})
