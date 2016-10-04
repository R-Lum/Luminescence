context("calc_AliquotSize")

set.seed(1)
temp <- calc_AliquotSize(
  grain.size = c(100,150), 
  sample.diameter = 1, 
  MC.iter = 100, 
  plot = FALSE,
  verbose = FALSE)

test_that("check class and length of output", {
  
  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 4)
  
})

test_that("check summary output", {
  
  expect_is(temp, c("RLum.Results", "RLum"))
  expect_equal(temp$args$grain.size, 125)
  expect_equal(temp$args$sample.diameter, 1)
  expect_equal(temp$args$packing.density, 0.65)
  expect_true(temp$args$MC)
  expect_equal(temp$args$grains.counted, NA)
  expect_equal(temp$args$MC.iter, 100)
})

test_that("check MC run", {
  
  expect_equal(round(temp$MC$statistics$n), 100)
  expect_equal(round(temp$MC$statistics$mean), 43)
  expect_equal(round(temp$MC$statistics$median), 39)
  expect_equal(round(temp$MC$statistics$sd.abs), 20)
  expect_equal(round(temp$MC$statistics$sd.rel), 45)
  expect_equal(round(temp$MC$statistics$se.abs), 2)
  expect_equal(round(temp$MC$statistics$se.rel), 5)
  expect_length(temp$MC$kde$x, 10000)
  expect_length(temp$MC$kde$y, 10000)
})