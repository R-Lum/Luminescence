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
                            cores = -1, multicore = TRUE),
               "'cores' should be a single positive integer value")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  SW({
  ## bootstrap
  expect_message(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1,
                              bootstrap = TRUE, bs.M = 10, bs.N = 5, bs.h = 5,
                              sigmab.sd = 0.04, debug = TRUE, log = FALSE,
                              multicore = TRUE, cores = 2),
                 "Applying the model to all replicates using 2 cores")
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
  })
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  set.seed(1)
  snapshot.tolerance <- 1.5e-6

  expect_snapshot_RLum(temp, tolerance = snapshot.tolerance)
  SW({
  expect_snapshot_RLum(calc_MinDose(data = ExampleData.DeValues$CA1 / 100,
                                    sigmab = 0.2, gamma.upper = 4, par = 4,
                                    log.output = TRUE, plot = FALSE),
                       tolerance = snapshot.tolerance)
  })
  expect_snapshot_RLum(calc_MinDose(data = ExampleData.DeValues$CA1,
                                    sigmab = 0.2, log = FALSE,
                                    verbose = FALSE, plot = FALSE),
                       tolerance = snapshot.tolerance)

  ## bootstrap = TRUE
  suppressWarnings( # Not enough bootstrap replicates for loess fitting
  expect_snapshot_RLum(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.1,
                                    bootstrap = TRUE, bs.M = 10, bs.N = 5,
                                    verbose = FALSE, plot = FALSE),
                       tolerance = snapshot.tolerance)
  )
  expect_snapshot_RLum(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.2,
                                    invert = TRUE, bootstrap = TRUE,
                                    bs.M = 20, bs.N = 5, bs.h = 10,
                                    verbose = FALSE, plot = FALSE),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(calc_MinDose(ExampleData.DeValues$CA1, sigmab = 2.1,
                                    bootstrap = TRUE, log = FALSE, par = 4,
                                    bs.M = 20, bs.N = 5, bs.h = 10,
                                    verbose = FALSE, plot = FALSE),
                       tolerance = snapshot.tolerance)
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
