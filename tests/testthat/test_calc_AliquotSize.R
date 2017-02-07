context("calc_AliquotSize")

set.seed(1)
temp <- calc_AliquotSize(
  grain.size = c(100,150),
  sample.diameter = 1,
  MC.iter = 100,
  plot = FALSE,
  verbose = FALSE)

test_that("check class and length of output", {
  testthat::skip_on_cran()
  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 2)
  expect_is(temp$summary, "data.frame")
  expect_is(temp$MC, "list")

})

test_that("check summary output", {
  testthat::skip_on_cran()
  result <- get_RLum(temp)

  expect_equal(result$grain.size, 125)
  expect_equal(result$sample.diameter, 1)
  expect_equal(result$packing.density, 0.65)
  expect_equal(result$n.grains, 42)
  expect_equal(result$grains.counted, NA)
})

test_that("check MC run", {
  testthat::skip_on_cran()
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
