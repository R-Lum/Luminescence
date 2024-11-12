## load data
data(ExampleData.MortarData, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  ## data
  expect_error(calc_EED_Model(),
               "'data' should be of class 'data.frame'")
  expect_error(calc_EED_Model(1L),
               "'data' should be of class 'data.frame'")

  ## expected dose
  expect_error(calc_EED_Model(MortarData),
               "'expected_dose' should be of class 'numeric'")
  expect_error(calc_EED_Model(MortarData, expected_dose = "error"),
               "'expected_dose' should be of class 'numeric'")

  ## IndivDose
  expect_error(calc_EED_Model(MortarData, expected_dose = 0.17,
                              MinIndivDose = "error"),
               "'MinIndivDose' should be of class 'numeric")
  expect_error(calc_EED_Model(MortarData, expected_dose = 0.17,
                              MaxIndivDose = "error"),
               "'MaxIndivDose' should be of class 'numeric")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## automated
  set.seed(1)
  SW({
  expect_s4_class(calc_EED_Model(MortarData, expected_dose = 11.7),
                  "RLum.Results")
  })

  ## TODO this leads to a crash and should be checked
  ## calc_EED_Model(data = MortarData, expected_dose = 11.7, n.simul = 1950)

  ## fast run
  expect_s4_class(calc_EED_Model(
    data = MortarData,
    kappa = 14,
    sigma_distr = 0.37,
    verbose = FALSE,
    expected_dose = 11.7), "RLum.Results")

  ## fast run, no plot, no terminal
  expect_s4_class(calc_EED_Model(
    data = MortarData,
    kappa = 14,
    sigma_distr = 0.37,
    verbose = FALSE,
    plot = FALSE,
    expected_dose = 11.7), "RLum.Results")

  ## more coverage
  SW({
  expect_s4_class(calc_EED_Model(
    data = MortarData,
    sigma_distr = 0.37,
    verbose = TRUE,
    method_control = list(trace = TRUE, trace_plot = TRUE),
    expected_dose = 11.7), "RLum.Results")
  })
})
