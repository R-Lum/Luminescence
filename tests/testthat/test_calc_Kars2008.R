context("calc_Kars2008")

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
kars <- calc_Kars2008(
  data = data,
  rhop = rhop,
  ddot = ddot,
  readerDdot = readerDdot,
  n.MC = 50,
  plot = FALSE
)

test_that("check class and length of output", {
  testthat::skip_on_cran()
  ##rhop
  expect_is(rhop, class = "RLum.Results", info = NULL, label = NULL)
    expect_is(rhop$fading_results, "data.frame")
    expect_is(rhop$fit, "lm")
    expect_is(rhop$rho_prime, "data.frame")

  ##kars
  expect_is(kars, class = "RLum.Results", info = NULL, label = NULL)
    expect_is(kars$results, class = "data.frame", info = NULL, label = NULL)
    expect_is(kars$data, class = "data.frame", info = NULL, label = NULL)
    expect_is(kars$Ln, class = "numeric", info = NULL, label = NULL)
    expect_is(kars$fits, class = "list", info = NULL, label = NULL)

})

test_that("check values from analyse_FadingMeasurement()", {
    expect_equal(round(sum(rhop$fading_results[,1:9]),0),415)
    expect_equal(round(sum(rhop$rho_prime),5),2e-05)
    expect_equal(round(sum(rhop$irr.times)), 2673108)

})

test_that("check values from calc_Kars2008()", {
  testthat::skip_on_cran()
  expect_equal(round(sum(kars$results),0), 2417)
  expect_equal(round(sum(kars$data),0), 191530)
  expect_equal(round(sum(kars$Ln),4), 0.1585)
  expect_equal(round(sum(residuals(kars$fits$simulated)),4),  1.2386)
  expect_equal(round(sum(residuals(kars$fits$measured)),4),  0.1894)
  expect_equal(round(sum(residuals(kars$fits$unfaded)),4),  1.6293)

})
