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
               "\"data\" is missing, with no default")
  expect_error(calc_Huntley2006("test"),
               "'data' must be a data frame")

  expect_error(calc_Huntley2006(data, fit.method = "test"),
               "Invalid fit option 'test'")
  expect_error(calc_Huntley2006(data, fit.method = "GOK", lower.bounds = 0),
               "Argument 'lower.bounds' must be of length 4")

  expect_error(calc_Huntley2006(data, LnTn = list()),
               "'LnTn' must be a data frame with 2 columns")
  expect_error(calc_Huntley2006(data, LnTn = data),
               "'LnTn' must be a data frame with 2 columns")
  expect_error(calc_Huntley2006(cbind(data, data), LnTn = data[, 1:2]),
               "When 'LnTn' is specified, the 'data' data frame must have")
  expect_error(calc_Huntley2006(cbind(data, data[, 1])),
               "The number of columns in 'data' must be a multiple of 3")

  expect_error(calc_Huntley2006(data, rhop = 1),
               "'rhop' must be a vector of length 2")
  expect_error(calc_Huntley2006(data, rhop = "test"),
               "'rhop' must be a numeric vector or an RLum.Results object")
  expect_error(calc_Huntley2006(data, rhop = rhop.test),
               "'rhop' accepts RLum.Results objects only if produced by")
  expect_error(calc_Huntley2006(data, rhop = c(-1, 4.9e-7)),
               "'rhop' must be a positive number")

  expect_error(calc_Huntley2006(data, rhop = rhop),
               "\"ddot\" is missing, with no default")
  expect_error(calc_Huntley2006(data, rhop = rhop, ddot = ddot),
               "\"readerDdot\" is missing, with no default")
  expect_error(calc_Huntley2006(data, rhop = rhop,
                                ddot = "test", readerDdot = readerDdot),
               "'ddot' and 'readerDdot' must be numeric vectors")
  expect_error(calc_Huntley2006(data, rhop = rhop,
                                ddot = ddot, readerDdot = "test"),
               "'ddot' and 'readerDdot' must be numeric vectors")

  SW({
  expect_warning(calc_Huntley2006(data[, 1:2], rhop = rhop, n.MC = 2,
                                  ddot = ddot, readerDdot = readerDdot,
                                  fit.method = "GOK"),
                 "'data' only had two columns")
  })
})

test_that("check class and length of output", {
  testthat::skip_on_cran()

  ##rhop
  expect_s4_class(rhop, class = "RLum.Results")
  expect_s3_class(rhop$fading_results, "data.frame")
  expect_s3_class(rhop$fit, "lm")
  expect_s3_class(rhop$rho_prime, "data.frame")

  ##kars
  expect_s4_class(huntley, class = "RLum.Results")
  expect_s3_class(huntley$results, class = "data.frame")
  expect_s3_class(huntley$data, class = "data.frame")
  expect_type(huntley$Ln, "double")
  expect_type(huntley$fits, "list")

})

test_that("check values from analyse_FadingMeasurement()", {
    expect_equal(round(sum(rhop$fading_results[,1:9]),0),415)
    expect_equal(round(sum(rhop$rho_prime),5),2e-05)
    expect_equal(round(sum(rhop$irr.times)), 2673108)

})

test_that("check values from calc_Huntley2008()", {
  testthat::skip_on_cran()

  expect_equal(round(huntley$results$Sim_Age, 1), 34)
  expect_equal(round(huntley$results$Sim_Age_2D0, 0), 175)
  expect_equal(round(sum(huntley$Ln),2), 0.16)

  expect_equal(round(sum(huntley$data),0), 191530)
  expect_equal(round(sum(residuals(huntley$fits$simulated)),1),  0.8)
  expect_equal(round(sum(residuals(huntley$fits$measured)),4),  0.1894)
  expect_equal(round(sum(residuals(huntley$fits$unfaded)),2),  0)
})

test_that("Further tests calc_Huntley2006", {
  testthat::skip_on_cran()

  ## check extrapolation
  set.seed(1)
  expect_s4_class(
    object = suppressWarnings(
      calc_Huntley2006(
        data = data,
        rhop = rhop,
        ddot = ddot,
        readerDdot = readerDdot,
        n.MC = 500,
        fit.method = "GOK",
        mode = "extrapolation",
        plot = FALSE, verbose = FALSE)),
  class = "RLum.Results")

  ## check force through origin EXP with wrong mode settings
  set.seed(1)
  expect_s4_class(
    object = suppressWarnings(
      calc_Huntley2006(
        data = data,
        rhop = rhop,
        ddot = ddot,
        readerDdot = readerDdot,
        n.MC = 500,
        fit.method = "EXP",
        fit.force_through_origin = TRUE,
        mode = "extrapolation",
        plot = FALSE,
        verbose = FALSE)),
    class = "RLum.Results")

  ## EXP ... normal
  set.seed(1)
  expect_s4_class(
    object = suppressWarnings(
      calc_Huntley2006(
        data = data,
        rhop = rhop,
        ddot = ddot,
        readerDdot = readerDdot,
        n.MC = 500,
        fit.method = "EXP",
        fit.force_through_origin = TRUE,
        mode = "interpolation",
        plot = FALSE,
        verbose = FALSE)),
    class = "RLum.Results")

  ## GOK normal
  set.seed(1)
  expect_s4_class(
    object = suppressWarnings(
      calc_Huntley2006(
        data = data,
        rhop = rhop,
        ddot = ddot,
        readerDdot = readerDdot,
        n.MC = 500,
        fit.method = "GOK",
        fit.force_through_origin = TRUE,
        mode = "interpolation",
        plot = FALSE,
        verbose = FALSE)),
    class = "RLum.Results")

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
    plot = FALSE,
    n.MC = 100),
    regexp = "\\[calc\\_Huntley2006\\(\\)\\] Ln\\/Tn is smaller than the minimum computed LxTx value.")
  })
})
