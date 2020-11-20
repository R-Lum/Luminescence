set.seed(1)
temp <- calc_FadingCorr(
  age.faded = c(0.1,0),
  g_value = c(5.0, 1.0),
  tc = 2592000,
  tc.g_value = 172800,
  n.MC = 100, verbose = FALSE)



test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_s4_class(temp, "RLum.Results")
  expect_equal(length(temp), 2)

  ##check the verbose mode
  expect_s4_class(calc_FadingCorr(
    age.faded = c(0.1,0),
    g_value = c(5.0, 1.0),
    tc = 2592000,
    tc.g_value = 172800,
    n.MC = 1, verbose = TRUE), class = "RLum.Results")

})

test_that("check values from output example 1", {
  testthat::skip_on_cran()
  local_edition(3)

  results <- get_RLum(temp)

  expect_equal(results$AGE, 0.1168)
  expect_equal(results$AGE.ERROR, 0.0035)
  expect_equal(results$AGE_FADED, 0.1)
  expect_equal(results$AGE_FADED.ERROR, 0)
  expect_equal(results$G_VALUE, 5.312393)
  expect_equal(round(results$G_VALUE.ERROR, 5), 1.01190)
  expect_equal(round(results$KAPPA, 3), 0.0230)
  expect_equal(round(results$KAPPA.ERROR,3), 0.004)
  expect_equal(results$TC, 8.213721e-05)
  expect_equal(results$TC.G_VALUE, 5.475814e-06)
  expect_equal(results$n.MC, 100)
  expect_equal(results$OBSERVATIONS, 100)
  expect_equal(results$SEED, NA)

})
