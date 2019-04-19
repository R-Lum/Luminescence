context("fit_ThermalQuenching()")

##create example data
data <- data.frame(
  T = c(25, 40, 50, 60, 70, 80, 90, 100, 110),
  V = c(0.06, 0.058, 0.052, 0.051, 0.041, 0.034, 0.035, 0.033, 0.032),
  V_X = c(0.012, 0.009, 0.008, 0.008, 0.007, 0.006, 0.005, 0.005, 0.004))


test_that("standard check", {
  testthat::skip_on_cran()

  ##trgger errors
  expect_error(fit_ThermalQuenching(data = "test"))

  ##simple run
  expect_s4_class(fit_ThermalQuenching(
    data = data,
    n.MC = NULL), class = "RLum.Results")

  ##simple run without plot etc
  expect_s4_class(fit_ThermalQuenching(
    data = data,
    verbose = FALSE,
    plot = FALSE,
    n.MC = NULL), class = "RLum.Results")


})

