## load data
set.seed(1)
data("ExampleData.Fading", envir = environment())
fading_data <- ExampleData.Fading$fading.data$IR50
data <- ExampleData.Fading$equivalentDose.data$IR50
ddot <- c(7.00, 0.004)
readerDdot <- c(0.134, 0.0067)

rhop <-
  analyse_FadingMeasurement(fading_data,
                            plot = FALSE,
                            verbose = FALSE,
                            n.MC = 10)
huntley <- calc_Huntley2006(
  data = data,
  rhop = rhop,
  ddot = ddot,
  readerDdot = readerDdot,
  n.MC = 50,
  plot = FALSE,
  verbose = FALSE
)

test_that("input validation", {
  testthat::skip_on_cran()

  rhop.test <- rhop
  rhop.test@originator <- "unexpected"

  expect_error(calc_Huntley2006(),
               "'data' should be of class 'data.frame'")
  expect_error(calc_Huntley2006("test"),
               "'data' should be of class 'data.frame'")
  expect_error(calc_Huntley2006(data.frame()),
               "'data' cannot be an empty data.frame")

  expect_error(calc_Huntley2006(data, fit.method = "test"),
               "'fit.method' should be one of 'EXP' or 'GOK'")
  expect_error(calc_Huntley2006(data, fit.method = "GOK", lower.bounds = 0),
               "'lower.bounds' should have length 4")

  expect_error(calc_Huntley2006(data, LnTn = list()),
               "'LnTn' should be of class 'data.frame'")
  expect_error(calc_Huntley2006(data, LnTn = data),
               "'LnTn' should be a data frame with 2 columns")
  expect_error(calc_Huntley2006(cbind(data, data), LnTn = data[, 1:2]),
               "When 'LnTn' is specified, 'data' should have only 2 or 3")
  expect_error(calc_Huntley2006(cbind(data, data[, 1])),
               "The number of columns in 'data' must be a multiple of 3")

  expect_error(calc_Huntley2006(data, rhop = 1),
               "'rhop' should have length 2")
  expect_error(calc_Huntley2006(data, rhop = "test"),
               "'rhop' should be of class 'numeric' or 'RLum.Results'")
  expect_error(calc_Huntley2006(data, rhop = rhop.test),
               "'rhop' accepts only RLum.Results objects produced by")
  expect_error(calc_Huntley2006(data, rhop = c(0, 4e-7)),
               "'rhop' must be a positive number, the provided value was 0 \u00B1 4e-07")

  expect_error(calc_Huntley2006(data, rhop = rhop),
               "'ddot' should be of class 'numeric'")
  expect_error(calc_Huntley2006(data, rhop = rhop, ddot = ddot),
               "'readerDdot' should be of class 'numeric'")
  expect_error(calc_Huntley2006(data, rhop = rhop,
                                ddot = "test", readerDdot = readerDdot),
               "'ddot' should be of class 'numeric'")
  expect_error(calc_Huntley2006(data, rhop = rhop,
                                ddot = 0.4, readerDdot = readerDdot),
               "'ddot' should have length 2")
  expect_error(calc_Huntley2006(data, rhop = rhop,
                                ddot = ddot, readerDdot = "test"),
               "'readerDdot' should be of class 'numeric'")
  expect_error(calc_Huntley2006(data, rhop = rhop,
                                ddot = ddot, readerDdot = 0.13),
               "'readerDdot' should have length 2")
  expect_error(calc_Huntley2006(data, rhop = rhop,
                                ddot = ddot, readerDdot = readerDdot,
                                n.MC = 0),
               "'n.MC' should be a positive integer scalar")
  expect_error(calc_Huntley2006(data, rhop = rhop,
                                ddot = ddot, readerDdot = readerDdot,
                                rprime = list()),
               "'rprime' should be of class 'numeric'")

  SW({
  expect_warning(calc_Huntley2006(data[, 1:2], rhop = rhop, n.MC = 2,
                                  ddot = ddot, readerDdot = readerDdot,
                                  fit.method = "GOK"),
                 "'data' has only two columns: we assume that the errors")
  })

  expect_warning(expect_error(
      calc_Huntley2006(data = data[1:20, ], rhop = rhop, ddot = ddot,
                       readerDdot = c(0.002, 0.003), n.MC = 2,
                       plot = FALSE, verbose = FALSE),
      "Simulated D0 is NA"),
      "Ln/Tn is smaller than the minimum computed LxTx value")

  expect_error(calc_Huntley2006(data = data[1:20, ], LnTn = data[1, c(2, 3)],
                                rhop = c(2, 2), ddot = c(7.00, 0.004),
                                readerDdot = c(0.01, 0.02), n.MC = 2,
                                verbose = FALSE),
               "Could not fit simulated curve, check suitability of model")
})

test_that("check class and length of output", {
  testthat::skip_on_cran()

  ##rhop
  expect_s4_class(rhop, class = "RLum.Results")
  expect_s3_class(rhop$fading_results, "data.frame")
  expect_s3_class(rhop$fit, "lm")
  expect_s3_class(rhop$rho_prime, "data.frame")
  expect_equal(round(sum(rhop$fading_results[,1:9]),0),415)
  expect_equal(round(sum(rhop$rho_prime),5),2e-05)
  expect_equal(round(sum(rhop$irr.times)), 2673108)

  ##kars
  expect_s4_class(huntley, class = "RLum.Results")
  expect_s3_class(huntley$results, class = "data.frame")
  expect_s3_class(huntley$data, class = "data.frame")
  expect_type(huntley$Ln, "double")
  expect_type(huntley$fits, "list")

  expect_equal(round(huntley$results$Sim_Age, 1), 34.10)
  expect_equal(round(huntley$results$Sim_Age_2D0, 0), 175)
  expect_equal(round(sum(huntley$Ln),2), 0.16)

  expect_equal(round(sum(huntley$data),0), 191530)
  expect_equal(round(sum(residuals(huntley$fits$simulated)),1),  0.8)
  expect_equal(round(sum(residuals(huntley$fits$measured)),4),  0.1894)
  expect_equal(round(sum(residuals(huntley$fits$unfaded)),2),  0)
})

test_that("Further tests calc_Huntley2006", {
  testthat::skip_on_cran()

  os <- tolower(Sys.info()[["sysname"]])
  snapshot.tolerance <- switch(os,
                               "linux" = 1.5e-5,
                               "darwin" = 8.0e-2,
                               "windows" = 8.0e-2)

  ## check extrapolation
  set.seed(1)
  expect_snapshot_RLum(
      calc_Huntley2006(
        data = data,
        rhop = rhop,
        ddot = ddot,
        readerDdot = readerDdot,
        n.MC = 100,
        fit.method = "GOK",
        mode = "extrapolation",
        plot = TRUE, verbose = FALSE),
      tolerance = snapshot.tolerance)

  ## check force through origin EXP with wrong mode settings
  set.seed(1)
  expect_snapshot_RLum(
      calc_Huntley2006(
        data = data,
        rhop = rhop,
        ddot = ddot,
        readerDdot = readerDdot,
        n.MC = 100,
        fit.method = "EXP",
        fit.force_through_origin = TRUE,
        mode = "extrapolation",
        plot = FALSE,
        verbose = FALSE),
      tolerance = max(snapshot.tolerance, 1.0e-2))

  ## EXP ... normal
  set.seed(1)
  expect_snapshot_RLum(
      calc_Huntley2006(
        data = data,
        rhop = rhop,
        ddot = ddot,
        readerDdot = readerDdot,
        n.MC = 100,
        fit.method = "EXP",
        fit.force_through_origin = TRUE,
        mode = "interpolation",
        plot = FALSE,
        verbose = FALSE),
      tolerance = snapshot.tolerance)

  ## GOK normal
  set.seed(1)
  expect_snapshot_RLum(
      calc_Huntley2006(
        data = data,
        rhop = rhop,
        ddot = ddot,
        readerDdot = readerDdot,
        n.MC = 100,
        fit.method = "GOK",
        fit.force_through_origin = TRUE,
        mode = "interpolation",
        plot = FALSE,
        verbose = FALSE),
      tolerance = if (os == "darwin") 1.7e-1 else max(snapshot.tolerance, 1.5e-3))

  ## check warning for failed fits
  ## dataset provided by Christine Neudorf
  df <- structure(list(V1 = c(0L, 0L, 200L, 800L, 1500L, 3000L, 8000L
  ), V2 = c(0.439, -0.046720922, 1.988131642, 7.577744961, 12.87699795,
            18.50187874, 32.72443771),
  V3 = c(0.029, 0.01269548, 0.090232208,
         0.322242141, 0.546595156, 0.739308178, 1.285979033)),
  class = "data.frame", row.names = c(NA, -7L))

  set.seed(1)
  SW({
  expect_warning(calc_Huntley2006(
    data = df,
    LnTn = NULL,
    rhop = c(0.0000121549740899913, 4.91596040125088E-07),
    ddot = c(6.96, 0.29),
    readerDdot = c(0.094, 0.01),
    normalise = FALSE,
    fit.method = "EXP",
    summary = TRUE,
    plot = TRUE,
    n.MC = 100),
    regexp = "\\[calc\\_Huntley2006\\(\\)\\] Ln\\/Tn is smaller than the minimum computed LxTx value.")
  })

  ## failing to fit unfaded EXP model
  ## test derived from data provided by Salome Oehler
  input <- data.frame(dose = c(0, 1550, 9300, 37200, 71500,
                               0, 1550, 9300, 37200, 71500),
                      LxTx = c(0.79, 4.67, 14.41, 26.59, 24.88,
                               0.95, 4.87, 14.17, 21.98, 25.12),
                      LxTx_error = c(0.002, 0.006, 0.01, 0.04, 0.02,
                                     0.002, 0.011, 0.02, 0.03, 0.03))
  set.seed(1)
  expect_error(calc_Huntley2006(input,
                                rhop = c(6.5e-06, 2.0e-08),
                                ddot = c(8.5, 1.5),
                                fit.method = "EXP",
                                mode = "extrapolation",
                                readerDdot = c(0.154, 0.1),
                                n.MC = 2),
               "Could not fit unfaded curve, check suitability of model and")

  ## more coverage
  expect_s4_class(
    calc_Huntley2006(
      data = data[1:10, ],
      LnTn = data[1:10, c(2, 3)],
      rhop = rhop, ddot = ddot, readerDdot = readerDdot,
      n.MC = 2, plot = TRUE, verbose = FALSE),
    class = "RLum.Results")
  expect_s4_class(
    calc_Huntley2006(
      data = data[1:10, ],
      LnTn = data[1, c(2, 3)],
      rhop = rhop, ddot = ddot, readerDdot = readerDdot,
      rprime = c(0.01, 2.2, length.out = 500),
      n.MC = 2, plot = FALSE, verbose = FALSE),
    class = "RLum.Results")
  ## set maxiter and trace
  expect_output(
    calc_Huntley2006(
      data = data[1:10, ],
      LnTn = data[1:10, c(2, 3)],
      rhop = rhop, ddot = ddot, readerDdot = readerDdot,
      n.MC = 2, plot = FALSE, verbose = FALSE, maxiter = 50, trace = TRUE))

  expect_error(
    calc_Huntley2006(
      data = iris[, 1:3],
      rhop = rhop, ddot = ddot, readerDdot = readerDdot,
      n.MC = 2, plot = FALSE, verbose = FALSE),
    "Unable to fit growth curve to measured data, try setting 'fit.bounds = FALSE'")
  expect_error(
    calc_Huntley2006(
      data = iris[, 1:3],
      rhop = rhop, ddot = ddot, readerDdot = readerDdot,
      n.MC = 2, plot = FALSE, verbose = FALSE, fit.bounds = FALSE),
    "Unable to fit growth curve to measured data$")
  expect_warning(expect_error(
    calc_Huntley2006(
      data = data,
      rhop = c(4e-5, 5e-7), ddot = c(8, 0.04), readerDdot = c(0.1, 0.006),
      n.MC = 2, fit.method = "GOK", plot = FALSE, verbose = FALSE),
    "Could not fit unfaded curve, check suitability of model and parameters"),
    "Ln is >10 % larger than the maximum computed LxTx value")
  expect_warning(expect_error(
    calc_Huntley2006(
      data = data,
      rhop = c(4e-5, 5e-7), ddot = c(8, 0.04), readerDdot = c(0.1, 0.006),
      n.MC = 2, mode = "extrapolation", plot = FALSE, verbose = FALSE),
    "Simulated D0 is NA: either your input values are unsuitable"),
    "Ln is >10 % larger than the maximum computed LxTx value")
  expect_error(
    calc_Huntley2006(
      data = data,
      rhop = c(1e-3, 1e-7), ddot = ddot, readerDdot = readerDdot,
      n.MC = 2, plot = FALSE, verbose = FALSE),
    "All simulated Lx/Tx values are identical and approximately zero")
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 660
  DRC <- data.frame(
      Dose = c(rep(0, 7), rep(500, 12), rep(1000, 6), rep(2000, 6)),
      Lx.Tx = c(1.79, rep(0.06, 6), rep(0.97, 12), rep(1.80, 6), rep(3.20, 6)),
      Lx.Tx.Err = c(0.038, rep(0.001, 6), rep(0.021, 12), rep(0.038, 6), rep(0.068, 6))
  )
  expect_s4_class(
      calc_Huntley2006(
          data = DRC,
          rhop = c(7.67e-7, 2.84e-7),
          fit.method = "GOK",
          readerDdot = c(0.0868, 0.005),
          ddot = c(2.372, 0.199),
      n.MC = 2, plot = FALSE, verbose = FALSE),
      "RLum.Results")

  ## issue 733
  expect_s4_class(
      calc_Huntley2006(data, rhop = c(4e-6, 5e-7), ddot = c(7, 0.004),
                       readerDdot = c(0.134, 0.0067), n.MC = 1,
                       mode = "extrapolation", verbose = FALSE),
      "RLum.Results")
})
