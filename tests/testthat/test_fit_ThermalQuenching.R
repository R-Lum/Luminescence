##create example data
data <- data.frame(
  T = c(25, 40, 50, 60, 70, 80, 90, 100, 110),
  V = c(0.06, 0.058, 0.052, 0.051, 0.041, 0.034, 0.035, 0.033, 0.032),
  V_X = c(0.012, 0.009, 0.008, 0.008, 0.007, 0.006, 0.005, 0.005, 0.004))

data_list <- list(data, data)
data_NA <- data
data_NA[1,] <- NA


test_that("standard check", {
  testthat::skip_on_cran()

  ##trigger errors
  expect_error(fit_ThermalQuenching(data = "test"))

  ##simple run with error
  expect_error(fit_ThermalQuenching(
    data = data[,1:2],
    n.MC = NULL), regexp = "'data' is empty or has less than three columns!")

  ##simple run with warning
  SW({
  expect_warning(fit_ThermalQuenching(
    data = cbind(data,data),
    n.MC = NULL), regexp = "data' has more than 3 columns, taking only the first three!")

  ##simple run with warning NA
  expect_warning(fit_ThermalQuenching(
    data = data_NA,
    n.MC = NULL), regexp = "NA values in 'data' automatically removed!")

  ##simple run
  expect_s4_class(fit_ThermalQuenching(
    data = data,
    n.MC = NULL), class = "RLum.Results")
  })

  ##simple run with fitting error
  expect_message(expect_null(fit_ThermalQuenching(
    data = data.frame(T = 1:10, V = 1:10, V_X = 1:10),
    n.MC = NULL)),
    "Error: Fitting failed, NULL returned")

  # ##switch off weights
  SW({
  expect_s4_class(fit_ThermalQuenching(
    data = data,
    method_control = list(weights = NULL),
    n.MC = NULL), class = "RLum.Results")

  ##simple list run
  expect_s4_class(fit_ThermalQuenching(
    data = data_list,
    n.MC = NULL), class = "RLum.Results")

  ##simple run without plot etc
  expect_s4_class(fit_ThermalQuenching(
    data = data,
    verbose = FALSE,
    plot = TRUE,
    n.MC = 10), class = "RLum.Results")
  })
})
