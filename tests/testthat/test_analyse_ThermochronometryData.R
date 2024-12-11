test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(analyse_ThermochronometryData(list()),
               "'object' should be of class 'character'")
  expect_error(analyse_ThermochronometryData("error"),
               "File does not exist")
  expect_error(analyse_ThermochronometryData(test_path("_data/CLBR.xlsx")),
               "XLS/XLSX format is not supported, use CSV instead")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  input.csv <- file.path(test_path("_data"),
                         paste0("CLBR_IR", c(50, 100, 150, 225), ".csv"))

  expect_s4_class(analyse_ThermochronometryData(input.csv[1]),
                  "RLum.Results")
})
