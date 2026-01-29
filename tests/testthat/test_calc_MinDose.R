## load data
data(ExampleData.DeValues, envir = environment())
temp <- calc_MinDose(data = ExampleData.DeValues$CA1,
                     sigmab = 0.1,
                     verbose = FALSE,
                     plot = FALSE)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_MinDose(),
               "'data' should be of class 'data.frame' or 'RLum.Results'")
  expect_error(calc_MinDose("test"),
               "'data' should be of class 'data.frame' or 'RLum.Results'")
  expect_error(calc_MinDose(data.frame()),
               "'data' cannot be an empty data.frame")
  expect_error(calc_MinDose(iris[, 1, drop = FALSE]),
               "'data' should have 2 columns")
  expect_error(calc_MinDose(ExampleData.DeValues$CA1),
               "'sigmab' should be a single positive value")
  expect_error(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 1, init.values = 1:4),
               "'init.values' should be of class 'list'")
  expect_error(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1,
                            init.values = list(1, 2, 3)),
               "Please provide initial values for all model parameters")
  expect_error(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1,
                            init.values = list(p0 = 0, p1 = 1, p2 = 2, mu = 3)),
               "Missing parameters: gamma, sigma")
  expect_error(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1, par = "error"),
               "'par' should be a single positive integer value")
  expect_error(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1, par = 2),
               "'par' can only be set to 3 or 4")
  expect_error(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1, level = 0),
               "'level' should be a single positive value")
  expect_error(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1,
                            invert = "error"),
               "'invert' should be a single logical value")
  expect_error(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1,
                            verbose = "error"),
               "'verbose' should be a single logical value")
  expect_error(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1,
                            debug = "error"),
               "'debug' should be a single logical value")
  expect_error(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1,
                            sigmab.sd = c(0.01, 0.02)),
               "'sigmab.sd' should be a single positive value")
  expect_error(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1,
                            bootstrap = TRUE, bs.M = -1),
               "'bs.M' should be a single positive integer value")
  expect_error(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1,
                            bootstrap = TRUE, bs.N = -1),
               "'bs.N' should be a single positive integer value")
  expect_error(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1,
                            bootstrap = TRUE, bs.h = -1),
               "'bs.h' should be a single positive value")
  expect_error(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1,
                            cores = -1),
               "'cores' should be a single positive integer value")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  SW({
  ## bootstrap
  expect_message(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1,
                              bootstrap = TRUE, bs.M = 10, bs.N = 5),
                 "Recycled Bootstrap")
  expect_message(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1,
                              bootstrap = TRUE, bs.M = 10, bs.N = 5, bs.h = 5,
                              sigmab.sd = 0.04, debug = TRUE, log = FALSE,
                              multicore = TRUE, cores = 2),
                 "bootstrap replicates using 2 cores")
  })

  ## RLum.Results object
  calc_MinDose(temp, sigmab = 0.1, verbose = FALSE, log = FALSE, par = 4,
               init.values = list(gamma = 54, sigma = 1, p0 = 0.01, mu = 70))

  ## missing values
  data.na <- ExampleData.DeValues$CA1
  data.na[1, 1] <- NA
  expect_message(calc_MinDose(data.na, sigmab = 0.1, verbose = FALSE),
                 "Input data contained NA/NaN values, which were removed")
  expect_message(expect_error(
      calc_MinDose(data.frame(data.na[, 1], NA)),
      "After NA removal, nothing is left from the data set"),
      "Input data contained NA/NaN values, which were removed")

  ## no converging fit
  skip_on_os("windows")
  set.seed(7)
  data.nofit <- data.frame(rep(4, 5), rnorm(5, 5))
  SW({
  expect_error(calc_MinDose(data.nofit, sigmab = 0.9, par=4),
               "Couldn't find a converging fit for the profile log-likelihood")
  })

  ## more coverage
  SW({
  expect_warning(calc_MinDose(ExampleData.DeValues$CA1 / 100, sigmab = 0.1,
                              par = 4, gamma.lower = 2, log.output = TRUE,
                              bootstrap = TRUE, bs.M = 10, bs.N = 5, bs.h=100),
                 "Gamma is larger than mu, consider running the model with new")
  expect_warning(calc_MinDose(ExampleData.DeValues$CA1[1:9, ], sigmab = 0.8,
                             bootstrap = TRUE, bs.M = 5, bs.N = 5, bs.h = 5,
                             debug = TRUE, log = FALSE),
                 "Not enough bootstrap replicates for loess fitting")
  expect_output(calc_MinDose(ExampleData.DeValues$CA1 / 100, sigmab = 0.1,
                             gamma.upper = 4,
                             verbose = TRUE, log.output = TRUE, par = 4))
  expect_silent(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1,
                             verbose = FALSE, invert = TRUE,
                             bootstrap = TRUE, bs.M = 10, bs.N = 5, bs.h = 10))
  })
})

test_that("check values from output example", {
  testthat::skip_on_cran()

  expect_s4_class(temp, "RLum.Results")
  expect_equal(length(temp), 9)

  results <- get_RLum(temp)
  expect_equal(round(results$de, digits = 5), 34.31834)
  expect_equal(round(results$de_err, digits = 6), 2.550964)
  expect_equal(results$ci_level, 0.95)
  expect_equal(round(results$ci_lower, digits = 5), 29.37526)
  expect_equal(round(results$ci_upper, digits = 5), 39.37503)
  expect_equal(results$par, 3)
  expect_equal(round(results$sig, digits = 2), 2.07)
  expect_equal(round(results$p0, digits = 8), 0.01053938)
  expect_equal(results$mu, NA)
  expect_equal(round(results$Lmax, digits = 5), -43.57969)
  expect_equal(round(results$BIC, digits = 4), 106.4405)
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  SW({
  vdiffr::expect_doppelganger("default",
                              calc_MinDose(ExampleData.DeValues$CA1,
                                           sigmab = 0.1))
  vdiffr::expect_doppelganger("invert",
                              calc_MinDose(ExampleData.DeValues$CA1,
                                           sigmab = 0.1,
                                           invert = TRUE))
  vdiffr::expect_doppelganger("small sigmab",
                              calc_MinDose(ExampleData.DeValues$CA1,
                                           sigmab = 0.009))
  })
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 898
  expect_silent(calc_MinDose(ExampleData.DeValues$CA1,
                             sigmab = 0.009, invert = TRUE,
                             verbose = FALSE, plot = FALSE))

  ## issue 900
  expect_warning(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1,
                              bootstrap = TRUE, bs.M = 1,
                              verbose = FALSE, plot = FALSE))

  ## issue 1332
  ## the seed was picked to get the smallest number of warnings and messages;
  ## this test relies on not using SW() to do its job
  set.seed(3)
  expect_warning(expect_warning(expect_message(
      calc_MinDose(data = data.frame(De = c(rnorm(4) + 5, -1),
                                     De_Err = rnorm(5) + 1),
                   sigmab = 1, log = TRUE, bootstrap = TRUE,
                   bs.M = 10, bs.N = 5, bs.h = 2, verbose = FALSE),
      "Unable to plot the likelihood profile for: p0"),
      "De values must be positive with 'log = TRUE', 1 values set to NA"),
      "Not enough bootstrap replicates for loess fitting, try increasing `bs.M`")
})
