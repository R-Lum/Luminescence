## load data
input.csv <- file.path(test_path("_data"),
                       paste0("CLBR_IR", c(50, 100, 150, 225), ".csv"))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(analyse_ThermochronometryData(list()),
               "'object' should be of class 'character'")
  expect_error(analyse_ThermochronometryData("error"),
               "File 'error' does not exist")
  expect_error(analyse_ThermochronometryData(input.csv[1], verbose = "error"),
               "'verbose' should be a single logical value")
  expect_error(analyse_ThermochronometryData(test_path("_data/CLBR.xlsx")),
               "File extension 'xlsx' is not supported, only 'csv' is valid")
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

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  SW({
  vdiffr::expect_doppelganger("default",
                              analyse_ThermochronometryData(input.csv[1]))
  })
})
