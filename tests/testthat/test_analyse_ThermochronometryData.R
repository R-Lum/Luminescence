## load data
input.csv <- file.path(test_path("_data"),
                       paste0("CLBR_IR", c(50, 100, 150, 225), ".csv"))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(analyse_ThermochronometryData(list()),
               "'object' should be of class 'character'")
  expect_error(analyse_ThermochronometryData("error"),
               "File does not exist")
  expect_error(analyse_ThermochronometryData(input.csv[1], verbose = "error"),
               "'verbose' should be a single logical value")
  expect_error(analyse_ThermochronometryData(test_path("_data/CLBR.xlsx")),
               "XLS/XLSX format is not supported, use CSV instead")
  expect_error(analyse_ThermochronometryData(test_path("_data/CLBR.xlsx")),
               "XLS/XLSX format is not supported, use CSV instead")
  expect_error(analyse_ThermochronometryData(input.csv[1], ITL_model = "error"),
               "'ITL_model' should be one of 'GOK' or 'BTS'")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_s4_class(analyse_ThermochronometryData(input.csv[1],
                                                verbose = FALSE),
                  "RLum.Results")
  SW({
  expect_s4_class(analyse_ThermochronometryData(input.csv[1], ITL_model = "BTS",
                                                verbose = TRUE,
                                                num_s_values_bts = 10),
                  "RLum.Results")
  })
})
