input.xls <- test_path("_data/CLBR.xlsx")
input.csv <- file.path(test_path("_data"),
                       paste0("CLBR_IR", c(50, 100, 150, 225), ".csv"))


test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(.import_ThermochronometryData(list()),
               "Input type not supported")
  expect_error(.import_ThermochronometryData("filename", output_type = "error"),
               "'output_type' should be one of 'RLum.Results' or 'list'")
  expect_error(.import_ThermochronometryData(input.xls),
               "XLS/XLSX format is not supported, use CSV instead")
  expect_error(.import_ThermochronometryData("error"),
               "File does not exist")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_snapshot_RLum(.import_ThermochronometryData(input.csv))
  expect_type(.import_ThermochronometryData(input.csv, output_type = "list"),
              "list")
})
