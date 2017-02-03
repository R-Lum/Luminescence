context("calc_gSGC")

set.seed(seed = 1)
temp <- calc_gSGC(data = data.frame(
  LnTn =  2.361, LnTn.error = 0.087,
  Lr1Tr1 = 2.744, Lr1Tr1.error = 0.091,
  Dr1 = 34.4),
  plot = FALSE,
  verbose = FALSE
  )



test_that("check class and length of output", {
  expect_is(temp, class = "RLum.Results", info = NULL, label = NULL)
  expect_is(temp$De, class = "data.frame", info = NULL, label = NULL)
  expect_is(temp$De.MC, class = "list", info = NULL, label = NULL)
  expect_equal(length(temp), 3)

})

test_that("check values from output example", {
  expect_equal(round(sum(temp$De), digits = 2), 30.39)
  expect_equal(round(sum(temp$De.MC[[1]]), 0), 10848)

})
