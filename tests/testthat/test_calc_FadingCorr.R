test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_FadingCorr("error"),
               "'age.faded' should be of class 'numeric' or 'integer'")
  expect_error(calc_FadingCorr(1),
               "'age.faded' should have length 2")
  expect_error(calc_FadingCorr(c(0.1, 0), "error"),
               "'g_value' should be of class 'numeric', 'integer' or 'RLum.Results'")
  expect_error(calc_FadingCorr(c(0.1, 0), 1),
               "'g_value' should have length 2")
  expect_error(calc_FadingCorr(age.faded = c(0.1, 0), g_value = c(5.0, 1.0)),
               "[calc_FadingCorr()] 'tc' should be of class 'numeric' or",
               fixed = TRUE)
  expect_error(calc_FadingCorr(age.faded = c(0.1, 0), g_value = c(5.0, 1.0),
                               tc = c(1000, 2000)),
               "[calc_FadingCorr()] 'tc' should be a positive scalar",
               fixed = TRUE)
  expect_error(calc_FadingCorr(age.faded = c(0.1, 0), g_value = c(5.0, 1.0),
                               tc = 2592000, n.MC = "error"),
               "'n.MC' should be a positive integer scalar")
  expect_error(calc_FadingCorr(age.faded = c(0.1, 0), g_value = c(5.0, 1.0),
                               tc = 2592000, interval = "error"),
               "'interval' should be of class 'numeric'")
  expect_error(calc_FadingCorr(age.faded = c(0.1, 0), g_value = c(5.0, 1.0),
                               tc = 2592000, interval = 1),
               "'interval' should have length 2")
  expect_error(calc_FadingCorr(age.faded = c(0.1, 0), g_value = c(5.0, 1.0),
                               tc = 2592000, txtProgressBar = "error"),
               "'txtProgressBar' should be a single logical value")
  expect_error(calc_FadingCorr(age.faded = c(0.1, 0), g_value = c(5.0, 1.0),
                               tc = 2592000, verbose = "error"),
               "'verbose' should be a single logical value")
})

test_that("check class and length of output", {
  testthat::skip_on_cran()

  ##check message
  expect_message(calc_FadingCorr(
    age.faded = c(6.404856, 0.51),
    g_value = c(17.5,1.42),
    tc = 462,
    n.MC = 100),
    "[calc_FadingCorr()] Error: No solution found, NULL returned: this usually",
    fixed = TRUE)

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
               "Unknown originator for the 'g_value' object provided")

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

  set.seed(1)
  temp <- calc_FadingCorr(
      age.faded = c(0.1,0),
      g_value = c(5.0, 1.0),
      tc = 2592000,
      tc.g_value = 172800,
      n.MC = 100, verbose = FALSE)

  expect_s4_class(temp, "RLum.Results")
  expect_equal(length(temp), 2)

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

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-4

  set.seed(1)
  expect_snapshot_RLum(calc_FadingCorr(
      age.faded = c(1, 0),
      g_value = c(5.0, 1.0),
      tc = 25920,
      tc.g_value = 172800,
      n.MC = 20, verbose = FALSE),
      tolerance = snapshot.tolerance)

  expect_snapshot_RLum(calc_FadingCorr(
      age.faded = c(10, 0),
      g_value = c(5.0, 1.0),
      tc = 2592000,
      tc.g_value = 172800,
      n.MC = 20, verbose = FALSE),
      tolerance = snapshot.tolerance)

  expect_snapshot_RLum(calc_FadingCorr(
      age.faded = c(1, 6),
      g_value = c(5.37778156709913, 0.70382588155986),
      tc = 378000, tc.g_value = 172800,
      n.MC = 1000, seed = 11, verbose = FALSE),
      tolerance = snapshot.tolerance)
})
