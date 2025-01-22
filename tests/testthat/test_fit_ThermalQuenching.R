##create example data
data <- data.frame(
  T = c(25, 40, 50, 60, 70, 80, 90, 100, 110),
  V = c(0.06, 0.058, 0.052, 0.051, 0.041, 0.034, 0.035, 0.033, 0.032),
  V_X = c(0.012, 0.009, 0.008, 0.008, 0.007, 0.006, 0.005, 0.005, 0.004))
data_NA <- data
data_NA[1,] <- NA

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(fit_ThermalQuenching(data = "test"),
               "'data' should be of class 'data.frame' or a 'list' of such objects")
  expect_error(fit_ThermalQuenching(
    data = data[,1:2],
    n.MC = NULL),
    "'data' is empty or has fewer than three columns")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ##simple run with warning
  SW({
  expect_warning(fit_ThermalQuenching(
    data = cbind(data,data),
    n.MC = NULL),
    "data' has more than 3 columns, taking only the first three")

  ##simple run with warning NA
  expect_warning(fit_ThermalQuenching(
    data = data_NA,
    n.MC = NULL),
    "NA values in 'data' automatically removed")

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

  SW({
  ## switch off weights
  expect_s4_class(fit_ThermalQuenching(
    data = data,
    method_control = list(weights = NULL),
    n.MC = NULL), class = "RLum.Results")

  ##simple list run
  expect_s4_class(fit_ThermalQuenching(
    data = list(data, data),
    n.MC = NULL), class = "RLum.Results")

  ##simple run without plot etc
  expect_s4_class(fit_ThermalQuenching(
    data = data,
    verbose = FALSE,
    plot = TRUE,
    n.MC = 10), class = "RLum.Results")
  })
})
