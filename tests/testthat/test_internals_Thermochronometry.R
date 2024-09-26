test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(.import_ThermochronometryData(list()),
               "Input type not supported")
  expect_error(.import_ThermochronometryData("filename", output_type = "error"),
               "'output_type' should be one of 'RLum.Results', 'list'")
  expect_error(.import_ThermochronometryData("error"),
               "`path` does not exist: 'error'") # from readxl
})

test_that("check functionality", {
  testthat::skip_on_cran()

  input <- test_path("_data/CLBR.xlsx")
  expect_snapshot_RLum(.import_ThermochronometryData(input))
  expect_type(.import_ThermochronometryData(input, output_type = "list"),
              "list")
})
