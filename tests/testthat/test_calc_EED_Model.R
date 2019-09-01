context("calc_EED_Model")

## load needed example data
data(ExampleData.MortarData, envir = environment())

test_that("break function", {
  testthat::skip_on_cran()

  ## data
  expect_error(calc_EED_Model(), regexp = "'data' needs to be a two-column data.frame, see manual!")
  expect_error(calc_EED_Model(1L), regexp = "'data' needs to be a two-column data.frame, see manual!")

  ## expected dose
  expect_error(calc_EED_Model(data = MortarData), regexp = "'expected_dose' is either missing or not of type numeric!")
  expect_error(calc_EED_Model(data = MortarData, expected_dose = "test"),
               regexp = "'expected_dose' is either missing or not of type numeric!")



})

test_that("successfull run", {
  testthat::skip_on_cran()

  ## automated
  set.seed(1)
  expect_s4_class(calc_EED_Model(data = MortarData, expected_dose = 11.7), "RLum.Results")

  ## fast run
  expect_s4_class(calc_EED_Model(
    data = MortarData,
    kappa = 14,
    sigma_distr = 0.37,
    expected_dose = 11.7), "RLum.Results")

  ## fast run, no plot, no terminal
  expect_s4_class(calc_EED_Model(
    data = MortarData,
    kappa = 14,
    sigma_distr = 0.37,
    verbose = FALSE,
    plot = FALSE,
    expected_dose = 11.7), "RLum.Results")

})
