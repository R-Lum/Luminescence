set.seed(1)
temp <- calc_FadingCorr(
  age.faded = c(0.1,0),
  g_value = c(5.0, 1.0),
  tc = 2592000,
  tc.g_value = 172800,
  n.MC = 100, verbose = FALSE)



test_that("check class and length of output", {
  testthat::skip_on_cran()

  ##trigger some errors
  expect_error(calc_FadingCorr(age.faded = "test", g_value = "test"),
    "\\[calc_FadingCorr\\(\\)\\] 'tc' needs to be set!")

  expect_error(
    calc_FadingCorr(age.faded = "test", g_value = "test", tc = 200),
    "\\[calc\\_FadingCorr\\(\\)\\] 'age.faded', 'g_value' and 'tc' need be of type numeric\\!")

  ##check message
  expect_message(calc_FadingCorr(
    age.faded = c(6.404856, 0.51),
    g_value = c(17.5,1.42),
    tc = 462,
    n.MC = 100), "\\[calc_FadingCorr\\(\\)\\] No solution found, return NULL. This usually happens for very large, unrealistic g-values")

  expect_s4_class(temp, "RLum.Results")
  expect_equal(length(temp), 2)

  ##check the verbose mode
  SW({
  expect_s4_class(calc_FadingCorr(
    age.faded = c(0.1,0),
    g_value = c(5.0, 1.0),
    tc = 2592000,
    tc.g_value = 172800,
    n.MC = 1, verbose = TRUE), class = "RLum.Results")

  ## g_value provided as RLum.Results object
  data("ExampleData.Fading", envir = environment())
  fading <- analyse_FadingMeasurement(ExampleData.Fading$fading.data$IR50,
                                      plot = FALSE)
  expect_s4_class(calc_FadingCorr(age.faded = c(0.1,0),
                                  g_value = fading, tc = 2592000),
                  "RLum.Results")
  })

  fading@originator <- "unexpected"
  expect_message(
      expect_null(calc_FadingCorr(age.faded = c(0.1,0),
                               g_value = fading, tc = 2592000)),
               "Unknown originator for the provided RLum.Results object")

  ## auto, seed (Note: this is slow!)
  SW({
  calc_FadingCorr(
    age.faded = c(0.1,0),
    g_value = c(5.0, 1.0),
    tc = 2592000,
    seed = 1,
    n.MC = "auto")
  })
})

test_that("check values from output example 1", {
  testthat::skip_on_cran()

  results <- get_RLum(temp)

  expect_equal(results$AGE, 0.1169)
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
