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

test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

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

  ##fix for different R versions
  if(R.version$major == "3" && as.numeric(R.version$minor) < 6){
    expect_equal(round(huntley$results$Sim_Age, 1), 41.3)
    expect_equal(round(huntley$results$Sim_Age_2D0, 0), 164)
    expect_equal(round(sum(huntley$Ln),4), 0.1585)

  }else{
    expect_equal(round(huntley$results$Sim_Age, 1), 34)
    expect_equal(round(huntley$results$Sim_Age_2D0, 0), 175)
    expect_equal(round(sum(huntley$Ln),2), 0.16)

  }


  expect_equal(round(sum(huntley$data),0), 191530)
  expect_equal(round(sum(residuals(huntley$fits$simulated)),1),  0.8)
  expect_equal(round(sum(residuals(huntley$fits$measured)),4),  0.1894)
  expect_equal(round(sum(residuals(huntley$fits$unfaded)),2),  0)

## COMPARE calc_Kars2008 (deprecated) vs. re-named calc_Huntley2006
test_that("compare deprecated calc_Kars2008 and calc_Huntley2006", {
  testthat::skip_on_cran()

  expect_identical({
    set.seed(1)
    calc_Huntley2006(
      data = data,
      rhop = rhop,
      ddot = ddot,
      readerDdot = readerDdot,
      n.MC = 50,
      fit.method = "EXP",
      plot = FALSE, verbose = FALSE)$results
  }, {
    set.seed(1)
    suppressWarnings(
    calc_Kars2008(
      data = data,
      rhop = rhop,
      ddot = ddot,
      readerDdot = readerDdot,
      n.MC = 50,
      fit.method = "EXP",
      plot = FALSE, verbose = FALSE)$results
    )
  })#EndOf::expect_identical

  expect_identical({
    set.seed(1)
    calc_Huntley2006(
      data = data,
      rhop = rhop,
      ddot = ddot,
      readerDdot = readerDdot,
      n.MC = 500,
      fit.method = "GOK",
      plot = FALSE, verbose = FALSE)$results
  }, {
    set.seed(1)
    suppressWarnings(
      calc_Kars2008(
        data = data,
        rhop = rhop,
        ddot = ddot,
        readerDdot = readerDdot,
        n.MC = 500,
        fit.method = "GOK",
        plot = FALSE, verbose = FALSE)$results
    )
  })#EndOf::expect_identical
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

