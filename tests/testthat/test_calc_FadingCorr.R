context("calc_FadingCorr")

set.seed(1)
temp<- calc_FadingCorr(g_value = c(3.3,0.03), tc = 752,
                           age.faded = c(100,10),
                           n.MCruns=100)




test_that("check class and length of output", {
  
  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 2)
  
})

test_that("check values from output example 1", {
  
  
  results <- get_RLum(temp)
  
  expect_equal(results$AGE, 144.61)
  expect_equal(results$AGE.ERROR, 14.17)
  expect_equal(results$AGE_FADED, 100)
  expect_equal(results$AGE_FADED.ERROR, 10)
  expect_equal(results$G_VALUE, 3.3)
  expect_equal(results$G_VALUE.ERROR, 0.03)
  expect_equal(results$TC, 2.384576e-8)
  expect_equal(results$N.MCRUNS, 100)
  expect_equal(results$OBSERVATIONS, 100)
  expect_equal(results$SEED, NA)
  
})
