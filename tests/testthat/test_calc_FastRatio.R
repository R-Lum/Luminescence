context("calc_FastRatio")

data("ExampleData.CW_OSL_Curve")
temp <- calc_FastRatio(ExampleData.CW_OSL_Curve, plot = FALSE, verbose = FALSE)


test_that("check class and length of output", {
  
  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 5)
  
})

test_that("check values from output", {
  
  
  results <- get_RLum(temp)
  
  expect_equal(round(results$fast.ratio, digits = 3), 405.122)
  expect_equal(round(results$fast.ratio.se, digits = 4), 119.7442)
  expect_equal(round(results$fast.ratio.rse, digits = 5), 29.55756)
  expect_equal(results$channels, 1000)
  expect_equal(round(results$channel.width, digits = 2), 0.04)
  expect_equal(results$dead.channels.start, 0)
  expect_equal(results$dead.channels.end, 0)
  expect_equal(results$sigmaF, 2.6e-17)
  expect_equal(results$sigmaM, 4.28e-18)
  expect_equal(results$stimulation.power, 30.6)
  expect_equal(results$wavelength, 470)
  expect_equal(results$t_L1, 0)
  expect_equal(round(results$t_L2, digits = 6), 2.446413)
  expect_equal(round(results$t_L3_start, digits = 5), 14.86139)
  expect_equal(round(results$t_L3_end, digits = 5), 22.29208)
  expect_equal(results$Ch_L1, 1)
  expect_equal(results$Ch_L2, 62)
  expect_equal(results$Ch_L3_start, 373)
  expect_equal(results$Ch_L3_end, 558)
  expect_equal(results$Cts_L1, 11111)
  expect_equal(results$Cts_L2, 65)
  expect_equal(round(results$Cts_L3, digits = 5), 37.66667)
  
})
