test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(fit_IsoThermalHolding(list()),
               "'data' should be of class 'character', 'RLum.Results' or")
  expect_error(fit_IsoThermalHolding("error"),
               "File does not exist")
  expect_error(fit_IsoThermalHolding(set_RLum("RLum.Results", data = list(1))),
               "'data' has unsupported originator")
  expect_error(fit_IsoThermalHolding(iris),
               "'data' has the wrong column headers")
  expect_error(fit_IsoThermalHolding(test_path("_data/CLBR.xlsx")),
               "XLS/XLSX format is not supported, use CSV instead")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  input.csv <- file.path(test_path("_data"),
                         paste0("CLBR_IR", c(50, 100, 150, 225), ".csv"))

  expect_s4_class(fit_IsoThermalHolding(input.csv[1], rhop = -7),
                  "RLum.Results")

  data <- .import_ThermochronometryData(input.csv[1])
  expect_s4_class(fit_IsoThermalHolding(data, rhop = -7, mfrow = c(2, 2)),
                  "RLum.Results")
})
