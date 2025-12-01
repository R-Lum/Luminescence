## load data
input.csv <- file.path(test_path("_data"),
                       paste0("CLBR_IR", c(50, 100, 150, 225), ".csv"))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(fit_IsothermalHolding(list()),
               "'data' should be of class 'character', 'RLum.Results' or")
  expect_error(fit_IsothermalHolding("error", rhop = 1e-7),
               "File 'error' does not exist")
  expect_error(fit_IsothermalHolding(set_RLum("RLum.Results", data = list(1)),
                                     rhop = 1e-7),
               "'data' has an unsupported originator")
  expect_error(fit_IsothermalHolding(iris, rhop = 1e-7),
               "'data' has the wrong column headers")
  expect_error(fit_IsothermalHolding(input.csv[1], rhop = 1e-4, ITL_model = "error"),
               "'ITL_model' should be one of 'GOK' or 'BTS'")
  expect_error(fit_IsothermalHolding(input.csv[1], rhop = "error"),
               "'rhop' should be of class 'numeric' or 'RLum.Results'")
  expect_error(fit_IsothermalHolding(input.csv[1], rhop = 1e-7, plot = "error"),
               "'plot' should be a single logical value")
  expect_error(fit_IsothermalHolding(input.csv[1], rhop = 1e-7, verbose = "error"),
               "'verbose' should be a single logical value")
  expect_error(fit_IsothermalHolding(test_path("_data/CLBR.xlsx"), rhop = 1e-7),
               "XLS/XLSX format is not supported, use CSV instead")
  expect_error(fit_IsothermalHolding(input.csv[1], rhop = 1e-7, ITL_model = "BTS",
                                     num_s_values_bts = 0),
               "'num_s_values_bts' should be a single positive integer value")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  SW({
  expect_s4_class(fit_IsothermalHolding(input.csv, rhop = 1e-7),
                  "RLum.Results")
  expect_s4_class(fit_IsothermalHolding(input.csv[2], rhop = 1e-5,
                                        ITL_model = "BTS",
                                        num_s_values_bts = 2),
                  "RLum.Results")

  data <- .import_ThermochronometryData(input.csv[1])
  expect_s4_class(fit_IsothermalHolding(data, rhop = 1e-7, mfrow = c(2, 2)),
                  "RLum.Results")

  expect_warning(expect_null(fit_IsothermalHolding(input.csv[1], rhop = 1e-1,
                                                   ITL_model = "BTS",
                                                   num_s_values_bts = 2)),
                 "No ITL model could be fitted")
  })
})

test_that("regression tests", {

  ## issue 652
  df <- data.frame(SAMPLE = rep("S1", 6),
                   TEMP = rep(150, 6),
                   TIME = c(1, 10, 30, 100, 300, 1000),
                   LxTx = c(5.323892, 5.235491, 5.186621, 4.893767, 4.272581, 3.064905),
                   LxTx_ERROR = rep(0.05, 6))

  res <- fit_IsothermalHolding(df, rhop = 1e-7, verbose = FALSE)
  expect_equal(res@data$coefs,
               data.frame(SAMPLE = "S1", TEMP = 150, A = NA_real_,
                          b = NA_real_, Et = NA_real_, s10 = NA_real_))

  ## issue 1003
  set.seed(5)
  expect_s4_class(fit_IsothermalHolding(input.csv[2], rhop = 1e-5,
                                        ITL_model = "BTS", verbose = FALSE,
                                        num_s_values_bts = 2),
                  "RLum.Results")
})
