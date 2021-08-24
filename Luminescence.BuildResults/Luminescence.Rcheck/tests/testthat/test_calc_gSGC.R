set.seed(seed = 1)
temp <- calc_gSGC(data = data.frame(
  LnTn =  2.361, LnTn.error = 0.087,
  Lr1Tr1 = 2.744, Lr1Tr1.error = 0.091,
  Dr1 = 34.4),
  plot = FALSE,
  verbose = FALSE
  )

test_that("plot and verbose and so", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_s4_class(calc_gSGC(data = data.frame(
    LnTn =  2.361, LnTn.error = 0.087,
    Lr1Tr1 = 2.744, Lr1Tr1.error = 0.091,
    Dr1 = 34.4),
    plot = TRUE,
    verbose = TRUE
  ), "RLum.Results")

})


test_that("test errors", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_error(calc_gSGC(data = NA))
  expect_error(calc_gSGC(data = data.frame(
    LnTn =  2.361, LnTn.error = 0.087,
    Lr1Tr1 = 2.744, Lr1Tr1.error = 0.091,
    Dr1 = 34.4),
    gSGC.type = 1,
    plot = FALSE,
    verbose = FALSE))

  expect_error(calc_gSGC(data = data.frame(a = 1, b = 1, c = 1, d = 1, e = 1, f = 1)))



})

test_that("check class and length of output", {
  testthat::skip_on_cran()
  expect_is(temp, class = "RLum.Results", info = NULL, label = NULL)
  expect_is(temp$De, class = "data.frame", info = NULL, label = NULL)
  expect_is(temp$De.MC, class = "list", info = NULL, label = NULL)
  expect_equal(length(temp), 3)

})

test_that("check values from output example", {
  testthat::skip_on_cran()
  expect_equal(round(sum(temp$De), digits = 2), 30.39)
  expect_equal(round(sum(temp$De.MC[[1]]), 0), 10848)

})
